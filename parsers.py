from typing import Dict, List, Set, Tuple, Optional
from dataclasses import dataclass
import re

@dataclass
class Production:
    left: str
    right: List[str]

    def __str__(self):
        return f"{self.left} -> {' '.join(self.right)}"

class ParseTreeNode:
    def __init__(self, value):
        self.value = value
        self.children = []
        self.rule_used = None  

    def add_child(self, child):
        self.children.append(child)

    def display(self, prefix="", is_last=True):
        connector = "└── " if is_last else "├── "
        rule_info = f" ({self.rule_used})" if self.rule_used else ""
        print(f"{prefix}{connector}{self.value}{rule_info}")
        prefix += "    " if is_last else "│   "
        for i, child in enumerate(self.children):
            child.display(prefix, is_last=(i == len(self.children) - 1))

class Grammar:
    def __init__(self):
        self.productions: List[Production] = []
        self.terminals: Set[str] = set()
        self.non_terminals: Set[str] = set()
        self.start_symbol: Optional[str] = None
        self.first_sets: Dict[str, Set[str]] = {}
        self.follow_sets: Dict[str, Set[str]] = {}

    def calculate_first_sets(self):
    
        self.first_sets = {nt: set() for nt in self.non_terminals}
        for t in self.terminals:
            self.first_sets[t] = {t}

        changed = True
        while changed:
            changed = False
            for prod in self.productions:
                current_first = self.first_sets[prod.left].copy()
                
                if not prod.right:  
                    if 'ε' not in self.first_sets[prod.left]:
                        self.first_sets[prod.left].add('ε')
                        changed = True
                    continue

                
                all_nullable = True
                for symbol in prod.right:
                    symbol_first = self.first_sets.get(symbol, {symbol})
                    self.first_sets[prod.left].update(symbol_first - {'ε'})
                    if 'ε' not in symbol_first:
                        all_nullable = False
                        break

                if all_nullable:
                    self.first_sets[prod.left].add('ε')

                if current_first != self.first_sets[prod.left]:
                    changed = True

    def calculate_follow_sets(self):
        self.follow_sets = {nt: set() for nt in self.non_terminals}
        self.follow_sets[self.start_symbol].add('$') 

        changed = True
        while changed:
            changed = False
            for prod in self.productions:
                for i, symbol in enumerate(prod.right):
                    if symbol in self.non_terminals:
                        current_follow = self.follow_sets[symbol].copy()
                        
                        
                        first_of_rest = set()
                        all_nullable = True
                        
                        for next_symbol in prod.right[i+1:]:
                            symbol_first = self.first_sets.get(next_symbol, {next_symbol})
                            first_of_rest.update(symbol_first - {'ε'})
                            if 'ε' not in symbol_first:
                                all_nullable = False
                                break
                                
                        self.follow_sets[symbol].update(first_of_rest)
                        
                       
                        if all_nullable:
                            self.follow_sets[symbol].update(self.follow_sets[prod.left])
                            
                        if current_follow != self.follow_sets[symbol]:
                            changed = True

    def is_ll1(self) -> bool:
        self.calculate_first_sets()
        self.calculate_follow_sets()

        
        for nt in self.non_terminals:
            first_sets_for_productions = []
            for prod in self.productions:
                if prod.left == nt:
                    first_of_rhs = set()
                    if not prod.right:  
                        first_of_rhs.add('ε')
                    else:
                        all_nullable = True
                        for symbol in prod.right:
                            symbol_first = self.first_sets.get(symbol, {symbol})
                            first_of_rhs.update(symbol_first - {'ε'})
                            if 'ε' not in symbol_first:
                                all_nullable = False
                                break
                        if all_nullable:
                            first_of_rhs.add('ε')
                    
                    for prev_first in first_sets_for_productions:
                        if first_of_rhs & prev_first:
                            return False
                    first_sets_for_productions.append(first_of_rhs)

        for prod in self.productions:
            if not prod.right:  
                if self.follow_sets[prod.left] & self.first_sets[prod.left]:
                    return False

        return True

    def parse_grammar_rules(self, grammar_string: str) -> None:
        lines = [line.strip() for line in grammar_string.split('\n') if line.strip()]

        for line in lines:
            if '→' in line:
                left = line.split('→')[0].strip()
                self.non_terminals.add(left)
                if not self.start_symbol:
                    self.start_symbol = left

        for line in lines:
            if '→' in line:
                left, right = map(str.strip, line.split('→'))
                alternatives = [alt.strip() for alt in right.split('|')]
                
                for alt in alternatives:
                    if alt == 'ε' or alt == 'ϵ':
                        self.productions.append(Production(left, []))
                    else:
                        symbols = self._tokenize_production(alt)
                        self.productions.append(Production(left, symbols))
                        for symbol in symbols:
                            if symbol not in self.non_terminals and len(symbol) == 1:
                                self.terminals.add(symbol)

    def _tokenize_production(self, production: str) -> List[str]:
        tokens = []
        current_token = ""
        
        for char in production:
            if char.isspace():
                if current_token:
                    tokens.append(current_token)
                    current_token = ""
            else:
                if len(current_token) == 1 or (current_token and current_token[-1].isupper() != char.isupper()):
                    if current_token:
                        tokens.append(current_token)
                    current_token = char
                else:
                    current_token += char
        
        if current_token:
            tokens.append(current_token)
            
        return tokens

    def display(self):
        print("\nGrammar:")
        for prod in self.productions:
            if prod.right:
                print(f"{prod.left} -> {' '.join(prod.right)}")
            else:
                print(f"{prod.left} -> ε")
        
        print("\nFIRST sets:")
        for symbol, first_set in self.first_sets.items():
            print(f"FIRST({symbol}) = {first_set}")
            
        print("\nFOLLOW sets:")
        for nt, follow_set in self.follow_sets.items():
            print(f"FOLLOW({nt}) = {follow_set}")

class RecursiveParser:
    def __init__(self, grammar: Grammar):
        self.grammar = grammar
        self.input = ""
        self.pos = 0
        self.memo = {} 

    def parse(self, input_string: str) -> Optional[ParseTreeNode]:
        self.input = input_string
        self.pos = 0
        self.memo = {}
        
        tree = self._parse_non_terminal(self.grammar.start_symbol, 0)
        if tree and self.pos == len(input_string):
            print("Successfully Parsed!")
            return tree
        else:
            raise ValueError(f"Parse Error: Could not parse entire input. Stopped at position {self.pos}")

    def _parse_non_terminal(self, non_terminal: str, start_pos: int) -> Optional[ParseTreeNode]:
        memo_key = (non_terminal, start_pos)
        if memo_key in self.memo:
            self.pos = self.memo[memo_key][1]
            return self.memo[memo_key][0]

        save_pos = self.pos
        node = ParseTreeNode(non_terminal)

        for prod in self.grammar.productions:
            if prod.left == non_terminal:
                self.pos = start_pos
                children = []
                success = True

                if not prod.right:
                    node = ParseTreeNode(non_terminal)
                    epsilon_node = ParseTreeNode('ε')
                    node.add_child(epsilon_node)
                    node.rule_used = f"{prod.left} -> ε"
                    self.memo[memo_key] = (node, self.pos)
                    return node

                for symbol in prod.right:
                    if symbol in self.grammar.non_terminals:
                        child = self._parse_non_terminal(symbol, self.pos)
                        if not child:
                            success = False
                            break
                        children.append(child)
                    else:  
                        if self.pos < len(self.input) and self.input[self.pos] == symbol:
                            children.append(ParseTreeNode(symbol))
                            self.pos += 1
                        else:
                            success = False
                            break

                if success:
                    node = ParseTreeNode(non_terminal)
                    for child in children:
                        node.add_child(child)
                    node.rule_used = str(prod)
                    self.memo[memo_key] = (node, self.pos)
                    return node

        self.pos = save_pos
        self.memo[memo_key] = (None, save_pos)
        return None

def main():
    grammar = Grammar()
    
    
    grammar_rules = """
S→abS|bX
X→ε|cN
N→cN'
N'→bN'|ε
"""
    
    grammar.parse_grammar_rules(grammar_rules)
    
    grammar.calculate_first_sets()
    grammar.calculate_follow_sets()
    
    if grammar.is_ll1():
        print("The grammar is LL(1)")
    else:
        print("The grammar is not LL(1)")
    
    grammar.display()

    parser = RecursiveParser(grammar)

    print("\n=== Parser Ready ===")
    while True:
        try:
            input_string = input("\nEnter a string to parse (or 'quit' to exit): ").strip()
            if input_string.lower() == 'quit':
                break
            if not input_string:
                print("Error: Empty input string")
                continue
            
            parse_tree = parser.parse(input_string)
            if parse_tree:
                print("\nParse Tree:")
                parse_tree.display()
            
        except ValueError as e:
            print(f"Error: {e}")
        except Exception as e:
            print(f"Unexpected error: {e}")

if __name__ == "__main__":
    main()
from typing import List
from fuzzywuzzy import process

def top_five_matches(query: str, choices: List[str]):
    top_matches = process.extract(query, choices, limit=5)
    return top_matches


import os
import json
import nltk
from fuzzywuzzy import fuzz
from nltk.corpus import wordnet

RULES_FILE = os.path.join(os.path.dirname(__file__), "rules.json")

# Expand input with synonyms from WordNet
def expand_keywords(keyword):
    synonyms = set()
    for syn in wordnet.synsets(keyword):
        for lemma in syn.lemmas():
            name = lemma.name().lower()
            if "_" not in name and name.isalpha():
                synonyms.add(name)
    return list(synonyms)

# Load all rules and flatten them with category preserved
def load_rules():
    if os.path.exists(RULES_FILE):
        with open(RULES_FILE, "r") as file:
            raw_data = json.load(file)
            flat_rules = []
            for group in raw_data:
                category = group.get("category", "general")
                for rule in group.get("rules", []):
                    rule_copy = rule.copy()
                    rule_copy["category"] = category
                    flat_rules.append(rule_copy)
            return flat_rules
    else:
        print(f"❌ {RULES_FILE} not found. Please create it with your base rules.")
        return []

# Save rules by grouping them by category
def save_rules(rules):
    grouped = {}
    for rule in rules:
        category = rule.get("category", "general")
        if category not in grouped:
            grouped[category] = []
        grouped[category].append({
            "keywords": rule["keywords"],
            "action": rule["action"]
        })

    output = [{"category": cat, "rules": rules} for cat, rules in grouped.items()]
    with open(RULES_FILE, "w") as file:
        json.dump(output, file, indent=4)

# Match user input to best rule
def match_problem(user_input, operative_rules):
    best_match = None
    best_score = 0
    threshold = 75

    for rule in operative_rules:
        for keyword in rule["keywords"]:
            score = fuzz.partial_ratio(user_input.lower(), keyword.lower())
            if score > best_score:
                best_score = score
                best_match = rule

    if best_score >= threshold:
        print(f"\n✅ Matched with score {best_score}%")
        print("Category:", best_match.get("category", "general"))
        print("Suggested action:", best_match["action"])
        return True
    else:
        print(f"\n❌ No strong match found (best score: {best_score}%)")
        return False

# Add a new rule with category support
def user_new_solution(user_input, operative_rules):
    MAX_SYNONYMS_PER_WORD = 5
    MAX_TOTAL_KEYWORDS = 10
    banned_words = {"christ", "god", "politics", "religion", "jesus", "within"}

    raw_keywords = input("📝 Enter keywords related to the issue (comma-separated):\n").strip().lower()
    keywords = [kw.strip() for kw in raw_keywords.split(",") if kw.strip()]

    if not keywords:
        print("⚠️ No keywords entered. Rule not saved.")
        return

    action = input("💡 What should the system suggest for this problem?\n").strip()
    if not action:
        print("⚠️ No action provided. Rule not saved.")
        return

    category = input("📂 Enter a category for this rule (e.g., video, audio, power):\n").strip().lower()
    if not category:
        category = "general"

    expanded = set()
    for kw in keywords:
        synonyms = expand_keywords(kw)
        filtered = [s for s in synonyms if s.isalpha() and " " not in s and len(s) < 15 and not any(bad in s.lower() for bad in banned_words)]
        expanded.update(filtered[:MAX_SYNONYMS_PER_WORD])

    expanded.update(keywords)
    limited_keywords = list(expanded)[:MAX_TOTAL_KEYWORDS]

    new_rule = {
        "category": category,
        "keywords": limited_keywords,
        "action": action
    }

    operative_rules.append(new_rule)
    save_rules(operative_rules)
    print(f"✅ New rule saved in category '{category}': {new_rule}")

# Main loop
def main():
    operative_rules = load_rules()

    while True:
        print("\nAI Troubleshooter")
        try:
            choice = int(input("1 - Describe issue\n2 - Add new rule\n3 - Exit\nYour choice: "))
        except ValueError:
            print("❌ Please enter a number.")
            continue

        match choice:
            case 1:
                available_categories = sorted(set(r.get("category", "Unknown") for r in operative_rules))
                print("📚 Available categories:")
                for cat in available_categories:
                    print(f" - {cat}")


                category_filter = input("📂 Optional: filter by category (or press enter to skip):\n").strip().lower()
                filtered_rules = [r for r in operative_rules if category_filter in r.get("category", "").lower()] if category_filter else operative_rules

                user_input = input("Describe your issue:\n").lower()
                matched = match_problem(user_input, filtered_rules)

                if not matched:
                    retry = input("No match. Would you like to rewrite your issue? (yes/no): ").strip().lower()
                    if retry == "yes":
                        user_input = input("Try again:\n").lower()
                        matched = match_problem(user_input, filtered_rules)

                    if not matched:
                        add = input("Still nothing. Want to add this as a new issue? (yes/no): ").strip().lower()
                        if add == "yes":
                            user_new_solution(user_input, operative_rules)
                            operative_rules = load_rules()
            case 2:
                user_input = input("Describe the new issue:\n").lower()
                user_new_solution(user_input, operative_rules)
                operative_rules = load_rules()
            case 3:
                print("👋 Exiting. Stay cool!")
                break
            case _:
                print("❌ Invalid option.")

if __name__ == "__main__":
    main()

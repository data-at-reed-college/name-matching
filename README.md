# Name matching package

The goal is to make a package for matching messy name files.

This will be based off a script I used in the Voter Matching folder. That one took names from voter contribution files and tried to find matches in the voter registry. It had only first name, last name, and zip code to go by. We're going to generalize that script and have different potential add on in addition to/instead of zip code.

## Current files

* three csvs with voter names and zip codes and campaign contribution data
* a voter registry file with names and zip codes and some other things
* a starter script

## Here's what the current script has so far

1. a part that cleans up the names (removes punctuation, remove titles like Mr. or Jr., squishes white space
2. a nickname list that Claude came up with
3. a part for getting exact matches
4. a part for tokenizing things so "Frankenstein, Baron Von" and "Von Frankenstein, Baron" can still be matched even though they have different parts that are listed as Lastname, Firstname
5. a part for matching by Jaro-Winkler cutoff (which measures string similarity)
6. a part for matching by Soundex similarity (John matches to Jon)
7. a part for matching including the nickname list
8. a "first word only" part that is theoretically supposed to look at rare last or first names and see how close the corresponding other part of the names are (supposed to be used for matching things like Dracula Frankenstein and D Frankenstein, but I don't think I finished seeing if it works right)
9. a part for combining matches and non-matches

## What it needs

1. a part for reading in files - there are example files for matching in this folder, all the year csvs can be matched to the voter_registry and can probably also be matched with each other; these are real data, so idk how well they should match
2. a part for specifying overlapping variables
   1. right now the script only has first name and last name from two datasets (df1 and df2) and the names have "first_key" and "last_key" for matching
   2. we want the names to be required but anything else can be add-ons used to help match, so zip code (we have that in the actual data), but in the future also things like state, county, year, etc; so we need an area in the script for specifying anything that overlaps between df1 and df2 and should be used for matching
3. a way to include the optional variables from part 2 into the existing functions. if they shouldn't be used as exact match criteria, then they would be useful for breaking ties or verifying close matches
4. a function that can be used at each step to summarize the number of matches that each step helped make (and anything else that might be useful in a summary)
5. to make all of this into a package
6. different things you think should be included, the user shouldn't necessarily have to use each type of matching thing, they can all be usable independently, like they can skip nicknames if they want (they should always look for exact matches though); add any other matchy things that you can think of that might be useful

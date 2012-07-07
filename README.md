# Huffinton Post API for R

Relatively simple connection to Huffpo's API
   
## What works now

- State polls can be downloaded and parsed into dataframes with the appropriate matching of responses to questions, pollsters, polls and topics. 
- We mostly ignore the "charts" and "chart" options and stick to "polls"
- National presidential polls are basically 60% there. 

## What may work in the future

- Graphs!

## Extras

- Some xPath helper functions like building expressions including conditionals. In the main we use json and ignore the original structure (as there are no attributes). But if you want to use XML that's there.
- A user friendly request generator exists.

## What you can do

- Send me nasty emails.
- Or pull requests
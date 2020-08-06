# Scratch

## Github Links

- https://docs.github.com/en/github/searching-for-information-on-github/understanding-the-search-syntax
- https://docs.github.com/en/github/searching-for-information-on-github/searching-issues-and-pull-requests
- https://docs.github.com/en/github/searching-for-information-on-github/sorting-search-results
- https://github.community/search?q=search
- https://docs.github.com/en/rest/reference/search#search-issues-and-pull-requests
- https://developer.github.com/v3/search/#ranking-search-results

## Sample Curls

Search for a PR that is:
- open
- has no reviews
- is not draft
- created after a specific date

```
curl -v -H 'Authorization: bearer --something-- https://github-provider/api/v3/search/issues?q=org:your_org+is:open+is:pr+review:none+draft:false+created:%3E2020-07-01
```

## Workflow

### Needed
- clone url
- working dir
- hash for PR ?

### Create

- mkdir -p <workdir>/:org/:repo/branch/hash
cd <workdir>/:org/:repo/branch/hash
git clone <clone url> -b branch .

### Run

//one script to run them all?
auto/test
sbt getCtags
menu
s .
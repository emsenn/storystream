# Storystream
*Stream textual narratives through TCP*

***Storystream*** is a simple socket server which allows people to connect (e.g. with a telnet client) and view stories. Most notable is that the *pacing* of each narrative is encoded within the **story** record. This dictates how fast lines are sent to the client.

## Usage


### Hosting
Hosting instructions coming soon

### Writing Stories
Stories [will be] text-files, where each line is either a string or a number. If the line is a string, it will be shown to a reader. If it is a number, it determines the delay before processing the next line.

```
The itsy bitsy spider
0.5
crawled up the water spout
1
Down came the rain
0.5
and washed the spider out
```

Would show a reader *The itsy bitsy spider*, wait half a second, then show them *crawled up the water spout*, and so on.

## Plans

- [0/9] Soon
  - [X] load stories from text files
  - [ ] break up storyreader into its own procedures
  - [ ] add story menu parser
  - [ ] add some lakota stories
  - [ ] clean up output
  - [ ] clean up console logs
  - [ ] play/pause on ENTER during narrative-play
  - [ ] quit during narrative-play
  - [ ] collect playtesters
- [0/4] Prerelease
  - [ ] write documentation
  - [ ] update README
  - [ ] prepare as Racket package
  - [ ] get playtester approval
- [0/6] Upgrades
  - [ ] render stories to plaintext (insert linebreaks based on pause duration)
  - [ ] add server stats & serialize them to disk (storyviews, reads, etc.)
  - [ ] allow more complex stories (lines that are lists (pick one at random) or lines that are procedures - these couldn't be recorded as just plaintext obvs) (this'll probably be version 2)
  - [ ] add story tags (content warnings)
  - [ ] add story length to its menu page
  - [ ] allow operators to tweak reading speed
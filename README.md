## Gift-Picker
A small tool to pair gift givers and recipients for Secret Santa

### Installing Racket
This program was written using Racket, a Scheme derivative.
You can download Racket [here](https://download.racket-lang.org/)
The installation is fairly straightforward, but I'd recommend making a shortcut to the Dr. Racket IDE that's included, as it's easiest to run the program there.

### Using this program
Clone or download the Gift-picker.scm file, and open in Dr. Racket.
Click the run button in the top left, this will open an Interactions Panel in the bottom half of the window.
Make sure the lower lefthand corner shows **Determine language from source** and switch it if it doesn't.
This program will take a list of names, a list of inclusions (forced pairings), and a list of exclusions.

It's helpful, but not necessary, to define each of these lists before calling them in the procedure.
This is done by entering into the interactions prompt `(define ...)`, followed by a shorthand name, and then the list itself.
As an example:

`(define TEST '(a b c d)`
or
```
(define TEST '(a
               b
               c
               d)
```

Lists in Scheme can be made easily either by using `(list ...)` or `'(...)` with spaces separating the entries
Nested lists (which we will need to use) can simply be added with interior parentheses, but if using `(list ...)`,
internal lists will need to use `(list )` or `'( )`, that is:
`(list (list 'a 'b) (list 'c 'd))`, `(list '(a b) '(c d))`, and `'((a b) (c d))` are all equivalent, the last form being recommended.

The list of names can be a simple list with spaces, but if you want to include last names, those entries will need to be their own nested list.
Example:

```
(define NAMES '(Spock
                Kirk
                (Jean Luc Picard)))
```

The inclusions and exclusions lists will need to be nested lists of name pairs.
For the inclusions, this will force the first person to give a gift to the second, and for the exclusions,
to not give a gift.

Valid inclusions and exclusions could look like:

``` 
(define INCLUDE '((Spock (Jean Luc Picard))
                  (Kirk Spock)))
```
or
```
(define EXCLUDE '((Spock Kirk)))
```

After doing this, the program can be called by entering:
`(gift-picker NAMES INCLUDE EXCLUDE)`
with the lists in that order. The names list is required but either or both of the others may be left out
by substituting an empty list as in `'()`.

Dr. Racket provides parentheses highlighting, which is helpful. 
Also as stated above, it's possible to just enter the lists in order in the `(gift-picker)` call,
but this is annoying to do. 

I'll likely spend some time to make this easier to use. Hopefully with a little more flexibility in the 
Inclusion/Exclusion lists and the ability to just run this on a text file. If things go particularly well,
I'll manage to make it work through Google Slides.

Thanks!

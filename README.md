# Calcify

A tiny static site generator for developers, written in [Racket](http://racket-lang.org).

Calcify is developed and supported by studio/studio.


## Install

Calcify runs with [Racket](http://racket-lang.org), a popular Scheme dialect and toolkit. Install Racket, if you don't have it, and make sure Racket's `/bin` directory is included in your `$PATH` (add it to your bash `~/.profile`).

To get Calcify itself:

    git clone git@github.com:twfarland/calcify.git
    cd calcify
    bash install.sh

Now include the `/{PATH-TO-CALCIFY}/bin/` in your `$PATH` variable (in your bash profile). You may need to restart your terminal to gain access to the `calcify` command.


## Using the `calcify` command

    Usage: calcify [options]
                                      
    With zero option flags, calcify generates static files in {CURRENT_DIRECTORY}/public 
                                      
    Options:
       -v       Print calcify's version
       -h       Display this help message
       -init    Initialize calcify project folders in the current directory
       -dev     Run the project in the current directory locally


## Example usage

### 1: Initialise a project

    mkdir calcify-example
    cd calcify-example
    calcify -init

This builds the following directory structure for your project:

    /assets    <- Base static files, these will be copied to /public upon generation
    /content   <- Your content .rkt files, from which the html will be generated
    /public    <- Where the static files are generated. Serve this with your production webserver
    /views     <- Your view .rkt files, where your layouts are defined

### 1: Create a layout view

Calcify uses simple s-expressions that are transformed to html. 
Create this file in your Calcify `/views` directory:

```scheme
;; layout.rkt:
(html
    (head (title (: title)))
    (body (h1 (: title))
          (menu (li (a (= href /) home))
                (li (a (= href /page2) Page 2)))
          (div (= id main) (: content))))
```

The head of an s-exp is usually the tag name. s-exps starting with `=` define tag attributes, and those starting with `:` define references to data being propagated the view.

### 2: Create some pages

A content file is a list of keys -> values.
Create these files in your Calcify `/content` directory:

```scheme
;; home.rkt:
((route    "")
 (view     "layout")
 (title    "Hello from Calcify")
 (content  (article (h3 Article title)
                    (p paragraph 1)
                    (p paragraph 2))))

;; page2.rkt:
((route    "page2")
 (view     "layout")
 (title    "Page 2")
 (content  (article (h3 Article 2 title)
                    (p paragraph 1)
                    (p paragraph 2))))
```

`route` defines the path to this page, both when served during development and when generated. `view` is the filename (without .rkt extension) of the view you want to use for this page. Every other key is optional, dependent on what you want to put in your views

### 3: Run the development server

    calcify -dev

Runs the site on `http://localhost:8080`, using Racket's webserver.
Each request checks the filesystem for changes. As such it is intended as a development convenience only.
Try altering your views or content, or adding new ones, then refresh the page. Your changes will be present.
Any css/js/images in `/assets` will be available in this server. 

*Beware:* don't work with files in the `/public` folder, as these will be overwritten when the site is generated.

### 4: Generate

When all looks good, you can run:

    calcify

This will create html files for the site in the `/public` folder. Point your production webserver (such as nginx) at this directory.

Make sure your production webserver renders `index.html` files as expected, i.e. `/somepath` refers to `/somepath/index.html`.

For convenience, You may like to run `calcify` as a Git post-receive hook in in your production environment.

That's it! 


## Full view syntax

Example content:

```scheme
((route   "")
 (view    "layout")
 (title   "Some title")
 (parent  (/parent-link "Parent title"))
 (tags    (("link1" "tag1") ("link2" "tag2") ("link3" "tag3"))))
 ```
#### Tags

Attributes use the `=` form.
Symbols and numbers are converted to strings.
Children are concatenated with a space. 
Every other s-exp will use its first item as the tag name:

```scheme
(br)
(div my div)
(a (= href /) (= class link) home)
(ul (li symbol) (li 1000) (li "some string"))

```
```html
<br>
<div>my div</div>
<a href="/" class="link">home</a>
<ul><li>symbol</li> <li>1000</li> <li>some string</li></ul>
```

Notes: 
- A tag s-exp must have children for its closing tag to be rendered. Just use a single empty string child for this purpose, i.e: `(script (= src /script.js) "")`
- Html5 is assumed, there are no self-closing tags
- Where all else fails, you can embed html as strings

#### References

```scheme
;; (: property)
(: title) -> "Some title"
(: non-existent) -> ""

;; (: property k1 ... kn)
(: tags 0) -> "tag1"
```

#### List comprehensions
```scheme
;; (-> (list value index) body)
(-> (tags tag i) (a (= href (: tag 0)) (: tag 1)))
```
```html
<a href="link1">tag1</a> <a href="link2">tag2</a> <a href="link3">tag3</a>
```

#### Existence conditionals

These are not full conditionals - they only check for existence. If no 'else' supplied, a blank string is returned.

```scheme
;; (? exists? then else)
(? parent (a (= href (: parent 0)) (: parent 1)))
```
```html
<a href="/parent-link">Parent title</a>
```


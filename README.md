# Calcify

A tiny (7kb) static site generator for developers, written in [Racket](http://racket-lang.org).

Calcify is developed and supported by studio/studio.



## Install

Calcify runs with [Racket](http://racket-lang.org), a popular Scheme dialect and toolkit. Install Racket, if you don't have it, and make sure Racket's `/bin` directory is included in your `$PATH` (add it to your bash `~/.profile`).

To get Calcify itself:

    git clone git://github.com/twfarland/calcify.git
    cd calcify
    raco exe src/generate.rkt
    raco exe src/develop.rkt

Calcify provides two programs:

### ./generate

Builds the static files in the `/public` folder. This is used in deployment.

### ./develop

Runs the site dynamically on `http://localhost:8080`. This is a development convenience.
When `/content` or `/view` folders are changed, the changes will be reflected without need for site regeneration.

### Directory structure

    /assets <- Any extra static files, these will be copied to /public upon generation
    /content <- Your content .rkt files, from which the html will be generated
    /public <- Where the static files are generated. Point your webserver at this
    /src <- The Calcify source files
    /views <- Your view .rkt files, where your layouts are defined
    generate <- The site generator program
    develop <- The site development program



## Example usage

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

The full view syntax is described at the bottom of this document.


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

`route` defines the path to this page, both when served during development and when generated. Every other key is optional, dependent on what you want to put in your views


## 3: Run the development server

    ./develop

This will run the site on `http://localhost:8080`. Visit it and see your rendered pages!
Try altering your views or content, or adding new ones, then refresh the page. Your changes will be present.
Any css/js/images in `/assets` will be available in this server. *Beware* don't work with those in the `/public` folder, as these will be overwritten when the site is generated.


## 4: Generate

When all looks good, you can run:

     ./generate  

This will create html files for the site in the `/public` folder. Point your production webserver (such as nginx) at this directory.

Make sure your production webserver renders `index.html` files as expected, i.e. `/somepath` refers to `/somepath/index.html`.

It works well to run `./generate` as a post-receive hook in Git.

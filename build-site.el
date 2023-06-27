;; Set the package installation directory so that packages aren't stored in the
;; ~/.emacs.d/elpa path.
(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize the package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install dependencies
(package-install 'htmlize)

;; Load the publishing system
(require 'ox-publish)

;; Customize the HTML output
(setq org-html-validation-link nil            ;; Don't show validation link
      org-html-head-include-scripts nil       ;; Use our own scripts
      org-html-head-include-default-style nil ;; Use our own styles
      org-html-head "<style type=\"text/css\">/* Base */html,body {    background-color: #222;    min-height: 100%;    line-height: 1.5;}body {    color: #fafafa;    font-family: \"Courier New\";}::selection {    background-color:  #2ecc40;    color: white;}/* Responsive content positioning */@media only screen and (min-width: 1020px) /* Large screens */{    body{        padding: 10vh 25vw;    }}@media only screen and (max-width: 1020px) and (min-width: 750px) /* Small screens */{    body{        padding: 5vh 10vw;    }}@media only screen and (max-width: 750px) /* Small screens */{    body{        padding: 2vh 5vw;    }}/* Headers */h1{font-size: 2.5rem;}h2{font-size: 1.7rem;}h1 > .subtitle, h3, h4, h5, h6{font-size: 1.3rem;}.title{    margin-bottom: 2.5rem;}/* Padding & Margin */* {margin: 0; padding: 0;}pre, blockquote, ul, ol, p, table{    margin: 1rem 0;}h1, h2{margin-top: 2rem; line-height: 2rem;}h3, h4, h5, h6{margin-top: 1rem;}/* Links  */a, a:visited {    color: #01ff70;    text-decoration: underline;}a:hover, a:focus, a:active {    color: #2ecc40;}/* Code */pre {    font-family: \"Courier New\";    padding: .5rem;    background-color: #333;    padding: 0.5rem;    border-radius: 0.2rem;    font-size: 0.9rem;    color: #EEE;    overflow-x: auto;}.org-keyword{    color: #01ff70;}.org-rainbow-delimiters-depth-1{    color: #2ecc40;}.org-rainbow-delimiters-depth-2{    color: #01ff70;}/* Blockquotes */blockquote {    border-left: 3px solid #01ff70;    padding-left: 1rem;}li{    list-style-position: inside;}/* Tags */.tag{    margin-top: 0.5rem;    display: block;    color: white;    font-size: var(--font-size-xsmall);}.tag > span{		font-weight: 400;    font-size: 0.8rem;    background-color: #444;    text-transform: uppercase;    border-radius: 2px;    width: fit-content;    height: auto;    padding: 1px 5px;}/* Keywords */.todo{    color: #2ecc40;}.done{    color: #444;}/* Overflows */.outline-text-2, .outline-text-3, .outline-text-4{	  max-width: 100%;	  overflow-x: auto;}/* Table */tr:nth-child(even) {    background-color: #333;}th, td{    padding: 0.5rem;    text-align: center;}.underline{    text-decoration: underline;}img{    max-width: 100%;    height: auto;}</style>")

;; Define the publishing project
(setq org-publish-project-alist
      (list
       (list "org-site:main"
             :recursive t
             :base-directory "./content"
             :publishing-function 'org-html-publish-to-html
             :publishing-directory "./"
             :with-author t             ;; Don't include author name
             :with-creator t            ;; Include Emacs and Org versions in footer
             :with-toc t                ;; Include a table of contents
             :section-numbers nil       ;; Don't include section numbers
             :time-stamp-file t)))      ;; Don't include time stamp in file

;; Generate the site output
(org-publish-all t)

(message "Build complete!")

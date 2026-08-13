# yetipad-app

This project is probably suitable for evaluation but is under active development.

I'll try and maintain released features and compatibility with saved content. The project includes automated tests, while the code remains subject to refactoring.

## Background
There are many notepad apps, I've never found one I'm really happy with. I don't have a good memory so I create lots of disorganised notes but finding them often becomes a task comparable to finding the source information. I need a way of easily organising and searching for things after the event.

I love to design and code in a good language, its great to have an app that can be easily enhanced with new features. I've followed Clojure and particularly ClojureScript since their release and seen them mature. Progressive Web Apps (PWA) also hold and appeal and should fit well with ClojureScript but for all the good bits I've always struggled with the development tool-stack and UI development experience. Two things promise to fix this: [Shadow-cljs] and [Material-UI].

Many years ago I used [Tiddlywiki] for keeping notes. For all its limitations it had some inspiring ideas, enough to enthuse me to create my own notepad app to evolve some of them. I wrote a desktop-browser app with offline caching as an early form of a PWA and used it as my notepad for many years, unfortunately it wasn't much good on mobile so didn't keep up with times and eventually fell into unusable disrepair, I resorted back to commercial offerings. Now in lock-down, in need of a new project and wanting to explore whats possible with material-ui, this is a new generation of the project taking shape.

## Main Features
1. Cross-platform, browser-based notepad application.
1. Google Drive document storage with synchronization, conflict handling, and automatic reconnect when the network returns.
1. Offline operation with local document and session persistence.
1. Search by title and content, with history and title index views.
1. Notes and tags, including hierarchical tag navigation and multiple tags per item.
1. Rich-text editing with selectable Google Closure, CKEditor, or Quill editors.
1. Progressive Web App assets and service-worker caching for the deployed application.

## Usage

Open the deployed application at [yetipad.milelo.uk](https://yetipad.milelo.uk). The application can create a new document from the document index, open an existing document from the index, or open a local `.edn`/`.odn` document file.

Google sign-in and Drive access are requested when needed. Browser pop-ups must be enabled for the Google sign-in window. The application starts locally from its cached document state and reconnects to Drive when the network is available.

Use the left drawer for tags, settings, trash, sign-in, logs, and the About page. Use the right-hand index drawer to search and browse documents by history, title, or content, and to create, open, move, or delete documents.

Document deletion asks for confirmation and moves the associated Drive file to trash. Items deleted from a document are placed in the in-app trash, where they can be restored or permanently deleted. Emptying the in-app trash also asks for confirmation.

While editing a note or tag, edit its title and rich-text content and add or remove tags. The Settings pane provides document title and subtitle, compression, sign-in email, content-editor selection, and sticky tag-bar options.

## Other features

1. Formatted HTML content and images can be pasted into the rich-text editors, subject to the capabilities of the selected editor.
1. Hyperlinks can be added from the editor and open in a new browser context.
1. The current document and open-item state are encoded in the URL, allowing browser refreshes, bookmarks, and history navigation to preserve the view.
1. Active editing sessions are persisted locally when the page is hidden or unloaded, and the browser warns before a reload that could lose unsaved changes.
1. The local store supports multiple browser tabs and keeps document state available while Drive is offline.
1. Drive synchronization reports offline, connecting, authorization-required, syncing, uploading, downloading, synchronized, connected, and error states in the application toolbar.
1. Documents use the app's restricted Google Drive scopes and are stored as the app's document files.

## Development features
1. Yetipad includes a trace logger and viewer; particularly useful for debugging on mobile devices.
1. shadow-cljs provides a built web server and hot reloading so app code changes will be rendered on saves or switching windows.
1. shadow-cljs generates minified and munged release builds using the ClojureScript / google Closure compiler for code optimisation and dead-code elimination.
1. shadow-cljs provides an nREPL server to dynamically change running code. VS Code with the [Calva plug-in] provides the editor and nREPL integration.

## App design features

* The document format is Clojure edn. The app retains embedded html as edn in hiccup format for easy processing and manipulation.
* The document root data structure is a map with keyword and string keys.
* The main user-added content uses string keys, these are base 36 numbers sequentially allocated. The values are maps as a minimum containing:
  * :id (same as key)
  * :kind the kind of content currently :note or :tag
  * :title
  * :create (time-date) utc iso time
  * :changed (time-date) utc iso time
  * :tags (tag-ids)

* There will be additional fields but they are :kind specific. Notes and tags have a :content field, containing hiccup format html.

* The app functionality associated with each :kind of item, is associated with the app with a registration process simplifying the addition of new content. 
* Registered fields include:
  * :kind - The kind keyword declaration
  * :icon - its icon
  * :pane - the pane rendering its content
  * :search - a custom search hook
  * :index-sort-order - to group :kind's in the index

## Development Environment setup

### Tooling

1. I use VS Code with the [Calva plug-in] for ClojureScript editing and REPL interaction.
1. ClojureScript is compiled with shadow-cljs and uses Reagent, Material UI, and npm packages.
1. Babashka runs the project tasks defined in `bb.edn`.

### Setup the IDE and tooling

* Install [VS Code] with the [Calva plug-in]
* Ensure you have these tools installed:
    * [Node.js] and npm
    * Java (the CI release build uses Java 21)
    * [Babashka] (`bb`)
    * git

### Setup the project

* `cd` to suitable install location.
* Clone the repo locally:

~~~
git clone https://github.com/milelo/yetipad-app.git
cd yetipad-app
npm install
~~~

* Open the project directory with your IDE.

### Start the development build:

* Open a terminal window in the IDE and execute:

~~~
bb dev
~~~

* This starts Shadow CLJS, its development web server, hot reload, and an nREPL server on port `8778` with cider-nrepl middleware enabled.
* Open a browser at `http://localhost:8282/`.
* Calva is preconfigured in `.vscode/settings.json` with the `Yetipad Shadow CLJS` connection sequence. Use `Connect to a running REPL server` and select that sequence; it reads the nREPL port from `.shadow-cljs/nrepl.port`, falls back to port `8778`, connects to the running server, and selects the `:app` build.
* Do not use `Start a REPL server` or jack-in for this workflow.

### Available build tasks

Run `bb <task>` from the project root:

* `bb compile` compiles the application build.
* `bb test` compiles the test build and runs `target/tests.js` with Node.js.
* `bb dev` starts the development watcher, web server, and nREPL.
* `bb build-sw` generates the service worker from the `docs` directory.
* `bb release` creates an optimized application build and service worker.
* `bb release-with-docs` creates the optimized docs build.
* `bb publish` creates and pushes the next automatically incremented release tag.
* `bb release-info [version]` displays the commit information for a release tag.

### Publish a release to GitHub Pages:

* Make sure the working tree is clean and the changes to be released are on `main`. `bb publish` enforces both conditions.
* Run `bb publish` to select and push the next automatically incremented `v1.x` tag.
* Pushing a `v*` tag dispatches the Pages workflow from `main`. The workflow checks out the tag, installs dependencies, copies `resources/public` to `docs`, builds the optimized application with source maps, sets the manifest version, generates the service worker, and deploys `docs` to GitHub Pages.

The workflow also supports manual dispatch with a required `release_tag` input. Supported values are current `v1.<minor>` tags and historical `v<number>` tags.

~~~
bb publish
~~~

The next `v1.x` tag is selected automatically after the latest existing release. Historical `vN` tags remain supported by `bb release-info` and the Pages workflow.

[Node.js]: https://nodejs.org/
[Babashka]: https://babashka.org/
[Tiddlywiki]: https://tiddlywiki.com/
[Calva plug-in]: https://calva.io/
[VS Code]: https://code.visualstudio.com/
[material-ui]: https://material-ui.com/
[shadow-cljs]: https://shadow-cljs.github.io/docs/UsersGuide.html#_introduction

---

This work is Copyright © 2020 Mike Longworth

<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons Licence" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.



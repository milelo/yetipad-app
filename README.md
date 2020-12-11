# yetipad-app

This project is probably suitable for evaluation but is under active development.

I'll try and maintain released features and maintain compatibility with saved content but the app isn't formally tested and the code is subject to refactoring.

## Background
There are many notepad apps, I've never found one I'm really happy with. I don't have a good memory so I create lots of disorganised notes but finding them often becomes a task comparable to finding the source information. I need a way of easily organising and searching for things after the event.

I love to design and code in a good language, its great to have an app that can be easily enhanced with new features. I've followed Clojure and particularly ClojureScript since their release and seen them mature. Progressive Web Apps (PWA) also hold and appeal and should fit well with ClojureScript but for all the good bits I've always struggled with the development tool-stack and UI development experience. Two things promise to fix this: [Shadow-cljs] and [Material-UI].

Many years ago I used [Tiddlywiki] for keeping notes. For all its limitations it had some inspiring ideas, enough to enthuse me to create my own notepad app to evolve some of them. I wrote a desktop-browser app with offline caching as an early form of a PWA and used it as my notepad for many years, unfortunately it wasn't much good on mobile so didn't keep up with times and eventually fell into unusable disrepair, I resorted back to commercial offerings. Now in lock-down, in need of a new project and wanting to explore whats possible with material-ui, this is a new generation of the project taking shape.

## Main Features
1. Cross platform compatibility - browser based
1. Automatic content syncing and merging between devices - using google drive
1. Off line capability - Progressive web app and local storage document caching.
1. Content and title searching, change-date ordered title index
1. A dynamically constructed hierarchical-menu for the content items (like notes) constructed from content tags. An item
   can have multiple parents.
1. A ClojureScript software and tooling stack. - my favourite language.
1. Fairly easy to add specialised viewers, editors and features.

## Usage

There are currently outstanding app issues with its google-drive authentication from some browsers. For the moment I
only recommend Chrome for desktop and particularly mobile. See the table below.

To load and open the app with a new document visit https://yetipad.mikelongworth.uk with your browser. **enable
pop-ups** to see the google-drive authentication window.

Note, this is the current release build of the project that is in active development without any formal testing. I will
maintain compatibility with saved content.

Browser | Desktop issues | Mobile issues
-|:-:|:-:
Chrome | ok | ok 
Edge | ok | -
*Firefox* | ok | authentication fails
*Safari* | authentication fails | authentication fails

* There are issues with google drive authentication with some browsers and authentication needs browser pop-ups to be
  enabled to support the google sign-in window. I think the problems are related to my server setup. Fixing these is on
  my to-do list; Unfortunately a lot of guesswork, trial and error and time is need to interface to google drive,
  particularly from JavaScript.
* On mobile browsers use the save desktop icon option in the browser menu to provide access as an app loaded with the
  current document.
* Documents initially display their weak-uuid as their title. Give the document a title using the settings pane; open it from the button near the bottom of the left drawer. I intend to enhance the rename functionality to automatically rename the drive-document-files to match the document title.
* The app doesn't have an explicit new-document function at the moment, navigate to the base url to create one, use the link on the 'about-app' page, or create your own note with a link to https://yetipad.mikelongworth.uk.
* All documents created with the app can be viewed and selected in the document index tab in the index drawer (right hand side drawer).
* Deleting documents doesn't request confirmation at the moment. To un-delete a document restore it from the trash in the google drive app. Google drive also keeps revisions, I've not evaluated this with the app yet to figure out the best way of utilising the feature.
* Deleting items moves them to in-apps trash. The empty trash button doesn't currently ask for confirmation.
* When editing items there is a tag editor underneath the main content area. Enter the text of a new tag and then enter to create a new tag or select an existing tag from the list. Tags appear as items like notes and potentially other content type. Tags can be added to other tags like they can be added to notes. Tags appear as entries to a hierarchical menu in the left drawer with untagged-tags (without parents) as the root entries.
* Alpha-numeric sorting is case sensitive so capital letters have higher priority. Good to start tags with a capital letter unless you want it low-order.

## Other features

1. Formatted web content including images can be pasted into editors, although the current editor won't necessarily have the capability to edit it.
1. Add hyperlinks to your content with the editor action. These will be opened in a new tab on desktop or on mobile platforms, in a browser window within the app.
1. The app maintains the open document and viewer-state encoded into its url allowing the view to be maintained with page refreshes, bookmarks and navigation through the browser history. Open-editors, or their edited content, aren't retained.
1. The app uses the browser local-store to sync between tabs so you can have multiple views of the same document open simultaneously. Changes are automatically saved when editors are closed. The content is synchronised on window focus or manually when the cloud icon is pressed. If you haven't any open editors you can also reload the app by swiping down from the top without losing the app state.
1. Document merging is supported . If there are items with conflicting changes (determined by the change date-time), both the Drive and local item will be retained, the local item is allocated a new id. **Note: synch conflicts with open editors aren't currently handled.** see local syncing...
1. Multiple instances in the same browser behave differently, they shouldn't get out of sync, the local-store is always available and kept in-sync however content in an open editor may be changed in another tab-instance causing a conflict. **This isn't currently handled** I intend to prevent the editor close with a warning with an option to save as a new item.
1. It may be possible to share a document with other people so they both have access to the same document in the app but I'm not sure what Drive supports with the access privileges of the app, there is a slim chance it may just work, I need to try it.
1. The app is restricted in its interactions with google-drive user content by it's credentials. I don't want the app to require approval from google. User only approved features are sufficient to allow it to create drive files and access the drive files the app has created. To stop unauthorised use of the credentials embedded in the app, they are restricted to use from the apps url.

## Development features
1. Yetipad includes a trace logger and viewer; particularly useful for debugging on mobile devices.
1. shadow-cljs provides a built web server and hot reloading so app code changes will be rendered on saves or switching windows.
1. shadow-cljs generates minified and munged release builds using the ClojureScript / google Closure compiler for code optimisation and dead-code elimination.
1. shadow-cljs provides an nREPL server to dynamically change running code. [Cursive] IDE pligin for [IntelliJ IDEA] provides an nREPL with the features of its code editor.
1. If the [Cursive] isn't suitable, VS Studio with the [Calva plug-in] is a good choice. I've used this before but not recent enough to provide guidance here.

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

## Development status

I've got a long list of tasks I'm chipping away at to make the app more complete and polished from both a user and developer point of view including a fair number of feature enhancements.

I'll maintain the document format, but I'm constantly refining the code.

Currently I embed the google closure editor, mainly because I've used it before with Reagent. I at least need to change its toolbar icons for material-ui icons but I also want to evaluate editor-alternatives like [Slate].

## Development Environment setup

### Tooling

1. I use the [Cursive] for the [IntelliJ IDEA] IDE. Cursive requires a licence. A free non-commercial licence can be obtained by email automatically on  request if you qualify. The IDE has been primarily designed for Clojure however its editing and repl works well with ClojureScript except for a few linting issues around javascript inter-op.
1. VSCode is also a good development option although at the time I evaluated it, some useful features are only provided by Cursive.
1. Reagent: I've used this and loved it since Dan Holmesand released it.
1. re-frame: great to keep your app in an MVC structure. As a personal preference my code doesn't conform faithfully to its prescribed event implementation. I feel the hoops it jumps through to promote functional purity makes the code more difficult to follow not easier.
1. shadow-cljs: Easy setup, dependencies management, build and hot reloading. js packages don't need any special treatment to support inter-op unlike other options. I use it with an external Clojure deps.edn dependencies file for compatibility with Cursive IDE and Clojure best practices.
1. venantius/accountant: for html history support.

I don't have a clean machine to ensure I've considered all the dependencies in the following setup guidelines. I'll update this in response to feedback.

### Setup the IDE and tooling

* Get an IDE preferably [Cursive] if not, VSCode with the [Calva plug-in]
* Ensure you have these tools installed:
    * nodejs - use its installer, or the [choco] package manager
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
npm run watch
~~~

* This should do everything to run the development code including building the code and starting the servers. It will also watch for changes to trigger and incremental build and refresh the browser.
* Open a Chrome browser with the url `http://localhost:8281/`
* Open the repl. I've pre-configured this in [Cursive]: `Run... > nREPL` or select direct from the run-combo in the toolbar.
* To switch to the cljs REPL select the pre-configured: `Tools > REPL > Commands > Start cljs nREPL` *Cursive Bug: the command isn't being imported correctly so use...*
* Or evaluate in the REPL:

~~~cljs
(shadow.cljs.devtools.api/nrepl-select :app)
~~~

### Build a release build:

* Stop the development build, you can ^C in the build window.

~~~
npm run release
~~~

* Build the service-worker to cache dependencies for off-line use:

~~~
node ./build-service-worker.js
~~~

* Copy the folder `./yetipad/` and its files to your server.
* To enable the App to access google Drive the server and url need to be white-listed with its google account. You could generate your own app-id and google drive credentials for your domain and update the app however the app won't then recognise documents created with its current app id. To support development http://localhost:8281/ and http://localhost:8000/ are white-listed. https://console.developers.google.com/apis/dashboard.

[Homebrew package manager]: https://brew.sh/
[Slate]: https://docs.slatejs.org/
[Tiddlywiki]: https://tiddlywiki.com/
[deps.edn]: https://clojure.org/reference/deps_and_cli
[Cursive]: https://cursive-ide.com/
[Calva plug-in]: https://calva.io/
[choco]: https://chocolatey.org/
[IntelliJ IDEA]: https://www.jetbrains.com/idea/download/
[material-ui]: https://material-ui.com/
[shadow-cljs]: https://shadow-cljs.github.io/docs/UsersGuide.html#_introduction

---

This work is Copyright © 2020 Mike Longworth

<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons Licence" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.



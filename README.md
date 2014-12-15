pem-images
==========

This program runs a list of statements on each image in a speicified folder.

Installation
============

1. Get the Haskell Platform 2013.2.0.0

https://www.haskell.org/platform/prior.html

2. Install wxWidgets 2.8.12 for Windows

http://sourceforge.net/projects/wxwindows/files/2.8.12/wxMSW-2.8.12-Setup.exe/download

3. Install this package

On a command line do `cabal install` in this directory

Usage
=====

![GUI Image](GUIImage.png?raw=true)

The screenshot shows how the GUI looks like. In the left half we see the list of statements. We add new statements with the buttons in the right half. Some statements have arguments that we can choose with the controls to the right of each button. The input folder has to contain image files. We select it after clicking the `Choose input path` button in the bottom right corner. We can delete a statement, save the list of statement, load them and run them with the buttons in the lower left corner right below the list of statements.

The statements come in three groups: Setting parameters, outputting things and adding another column to an output table.

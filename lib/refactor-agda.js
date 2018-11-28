'use babel';

import StringInputPanel from './string-input-panel';
import {CompositeDisposable, Disposable} from 'atom';
import ErrorPanel from './error-panel';

export default {

  stringInputPanel: null,
  modalPanel: null,
  subscriptions: null,
  errorPanel: null,

  activate(state) {

    this.stringInputPanel = new StringInputPanel(state.stringInputPanel);
    this.modalPanel = atom.workspace.addModalPanel({
      item: this.stringInputPanel.getElement(),
      visible: false
    });

    // Events subscribed to in atom's system can be easily cleaned up with a CompositeDisposable
    this.subscriptions = new CompositeDisposable();

    this.subscriptions.add(atom.commands.add('atom-text-editor', {
      'refactor-agda:rename': () => this.rename()
    }));

    this.subscriptions.add(atom.commands.add('atom-text-editor', {'refactor-agda:push-argument': () => this.pushArgument()}));

    this.subscriptions.add(atom.commands.add('atom-text-editor', {'refactor-agda:extract-function': () => this.extractFunction()}));

    this.stringInputPanel.getElement().children[1].addEventListener('blur', () => this.hideModal());

    this.errorPanel = atom.workspace.addBottomPanel({item: new ErrorPanel, visible:false});


  },

  deactivate() {
    this.modalPanel.destroy();
    this.subscriptions.dispose();
    this.stringInputPanel.destroy();
    this.errorPanel.destroy();
  },

  showModal() {
    this.modalPanel.show();
  },

  hideModal(){
    this.modalPanel.hide();
    const editor = atom.workspace.getActiveTextEditor();
    editor.element.focus();
    this.stringInputPanel.setAnswer('');
  },

  setUpRefactoring(doRefactoring){
    const editor = atom.workspace.getActiveTextEditor();
    if (editor) {
      const path = require('path');
      var fileName = editor.getPath();
      if (!fileName){
        this.errorPanel.getItem().setText("Save your file first.");
        this.errorPanel.show();
        return;
      } else
        if (!fileName.endsWith('.agda')){
          this.errorPanel.getItem().setText("This only works on Agda files.");
          this.errorPanel.show();
          return;
        }else {
          doRefactoring();
        };
    } else {
      this.errorPanel.getItem().setText("Open a file.");
      this.errorPanel.show();
      return;
    }
  },

  callEngine(refactoring){
    const editor = atom.workspace.getActiveTextEditor();
    var execSync = require('child_process').execSync;
    var filePath = editor.getPath();
    const path = require('path');
    var curr = path.resolve(__dirname);
    try {
      console.log("callEngine was called!");
      editor.save();
      var stdoutbuffer = execSync(`cabal run ${filePath} ${refactoring}`, {cwd: `${curr}/..`, timeout: 1000000});
      var a = stdoutbuffer.toString().split(`start of RefactorAgda output`);
      editor.setText(a[1]);
      this.errorPanel.hide();
      }
      catch (error){
        var a = error.message.split(`start of RefactorAgda output`);
        this.errorPanel.getItem().setText(a[1]);
        this.errorPanel.show();
        console.log(error.message);
        return;
        //error panel
        //this.ghcsjstryView.element.children[0].textContent = error.message;
        //error.status;  // Might be 127 in your example.
        //error.message; // Holds the message you typically want.
        //error.stderr;  // Holds the stderr output. Use `.toString()`.
        //error.stdout;  // Holds the stdout output. Use `.toString()`.
      };
  },

  offset(){
    const editor = atom.workspace.getActiveTextEditor();
    var point = editor.getCursorBufferPosition();
    return editor.getBuffer().characterIndexForPosition(point);
  },

  pushArgument(){
    this.setUpRefactoring(() => {
      this.callEngine(`push ${this.offset()}`);
    });
  },

  rename() {
    this.setUpRefactoring(() => {
        this.stringInputPanel.setQuestion("New name: ");
        this.showModal();
        this.stringInputPanel.setOnKeyUp( event => {
            if (event.key === 'Enter'){
              var newName = this.stringInputPanel.getAnswer();
              this.hideModal();
              this.callEngine(`rename ${this.offset()} ${newName}`);
            }});
      });
  },

  extractFunction() {
    console.log('ExtractFunction was called')
    this.setUpRefactoring(() => {
      const editor = atom.workspace.getActiveTextEditor();
      var range = editor.getSelectedBufferRange();
      var start = editor.getBuffer().characterIndexForPosition(range.start);
      var end = editor.getBuffer().characterIndexForPosition(range.end);
      this.callEngine(`extractFunction ${start} ${end}`);
    });
  },

  toggle() {
    console.log('RefactorAgda was toggled!');
    return (
      this.modalPanel.isVisible() ?
      this.modalPanel.hide() :
      this.modalPanel.show()
    );
  }

};

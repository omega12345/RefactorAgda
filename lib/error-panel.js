'use babel';

export default class ErrorPanel {

  constructor(serializedState) {
    // Create root element
    this.element = document.createElement('div');
    this.element.classList.add('refactor-agda');

    // Create message element
    const message = document.createElement('div');
    message.textContent = 'Failed to set error message';
    message.classList.add('message');
    this.element.appendChild(message);
  }

  // Returns an object that can be retrieved when package is activated
  serialize() {}

  // Tear down any state and detach
  destroy() {
    this.element.remove();
  }

  setText(message){
    this.element.children[0].textContent = message;
  }

  getElement() {
    return this.element;
  }
//This stuff is for using as a main component
  getTitle() {
    // Used by Atom for tab text
    return 'RefactorAgda\'s error panel';
  }

  getURI() {
    // Used by Atom to identify the view when toggling.
    return 'atom://refactor-agda-error';
  }

  getDefaultLocation() {
    // This location will be used if the user hasn't overridden it by dragging the item elsewhere.
    // Valid values are "left", "right", "bottom", and "center" (the default).
    return 'bottom';
  }

  getAllowedLocations() {
    // The locations into which the item can be moved.
    return ['left', 'right', 'bottom'];
  }

}

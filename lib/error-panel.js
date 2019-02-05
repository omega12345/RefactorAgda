'use babel';

export default class ErrorPanel {

  constructor(serializedState) {
    // Create root element
    this.element = document.createElement('div');
    this.element.classList.add('refactor-agda');

    // Create message element
    const message = document.createElement('div');
    this.element.appendChild(message);
    message.classList.add('message');
    this.setError('Failed to set error message');
    message.style.overflow = "auto";

    //message.style.backgroundColor = "red";
    message.style.fontSize = "medium";
    message.style.height = "50px";

    //message.style.color = "blue";

  }

  // Returns an object that can be retrieved when package is activated
  serialize() {}

  // Tear down any state and detach
  destroy() {
    this.element.remove();
  }

  setError(message){
    const box = this.element.children[0];
    box.textContent = message;
    box.style.fontSize = "large";
    box.style.height = "150px";
    box.style.color = "red";
  }

  setText(message){
    //this.element.children[0].textContent = message;
    //this.element.children[0].
    const box = this.element.children[0];
    box.textContent = message;
    box.style.fontSize = "medium";
    box.style.height = "30px";
    box.style.overflow = "auto";
    box.style.color = "blue";
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

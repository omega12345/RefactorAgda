'use babel';

export default class StringInputPanel {

  constructor(serializedState) {
    // Create root element
    this.element = document.createElement('div');
    this.element.classList.add('refactor-agda');

    // Create message element
    const question = document.createElement('div');
    question.textContent = 'Forgot what info to ask for';
    question.classList.add('question');
    this.element.appendChild(question);
    const input = document.createElement('input');
    input.setAttribute("type", "text");
    this.element.appendChild(input)
  }

  // Returns an object that can be retrieved when package is activated
  serialize() {}

  // Tear down any state and detach
  destroy() {
    this.element.remove();
  }

  getElement() {
    return this.element;
  }

  setQuestion(q){
    this.element.children[0].textContent = q;
  }

  setOnKeyUp(func){
    clone = this.element.children[1].cloneNode(true);
    this.element.replaceChild(clone, this.element.children[1]);
    this.element.children[1].addEventListener('keyup', func);
    clone.focus();
  }

  getAnswer(){
    return this.element.children[1].value;
  }

  setAnswer(ans){
    this.element.children[1].value = ans;
  }

}

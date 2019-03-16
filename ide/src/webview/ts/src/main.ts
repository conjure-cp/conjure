import Globals from './testable/Globals';
import Keyboard from './util/Keyboard';
import Listener from './testable/Listener';
import Buttons  from './util/Buttons';

Listener.bindListener();
Globals.initialize(Globals.vscode);
// Globals.loadNNodes();
Keyboard.bindKeys();
Buttons.bindButtons();
console.log("HELLO");
import Globals from './modules/Globals';
import Keyboard from './controls/Keyboard';
import Listener from './modules/Listener';
import Buttons  from './controls/Buttons';

Listener.bindListener();
Globals.initialize(Globals.vscode);
// Globals.loadNNodes();
Keyboard.bindKeys();
Buttons.bindButtons();
console.log("HELLO");
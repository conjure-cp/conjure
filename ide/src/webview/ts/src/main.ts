import Globals from './util/Globals';
import Keyboard from './util/Keyboard';
import Listener from './util/Listener';
import Buttons  from './util/Buttons';

Listener.bindListener();
Globals.initialize();
// Globals.loadNNodes();
Keyboard.bindKeys();
Buttons.bindButtons();
console.log("HELLO");
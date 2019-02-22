import Globals from './util/Globals';
import Keyboard from './util/Keyboard';
import Listener from './util/Listener';

Listener.bindListener();
Globals.initialize();
Globals.loadNNodes();
Keyboard.bindKeys();
console.log("HELLO");


import Globals from './util/Globals';
import Keyboard from './util/Keyboard';
import Listener from './util/Listener';
import Tree from './util/Tree';


console.log("HELLO");

// Globals.setup(Tree.zoom);
Keyboard.bindKeys();
Listener.bindListener();
Globals.loadNNodes();
Globals.selectNode(Globals.data.selectedId);
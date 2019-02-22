

import Globals from './util/Globals';
import Keyboard from './util/Keyboard';
import Listener from './util/Listener';
import Tree from './util/Tree';


console.log("HELLO");

// Globals.setup(Tree.zoom);
Globals.initialize();
Keyboard.bindKeys();
Listener.bindListener();
Globals.loadNNodes();
Tree.selectNode(Globals.data.selectedId);
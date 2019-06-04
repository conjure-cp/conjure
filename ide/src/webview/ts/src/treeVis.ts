import Globals from './modules/Globals';
import Keyboard from './controls/Keyboard';
import Listener from './modules/Listener';
import Buttons from './controls/Buttons';
import * as Web from './modules/Web';


$("#wantSVG").prop("checked",true);

Listener.bindListener();
Globals.initialize(Globals.vscode);
Keyboard.bindKeys();
Buttons.bindButtons();

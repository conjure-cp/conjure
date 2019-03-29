import Globals from './modules/Globals';
import Keyboard from './controls/Keyboard';
import Listener from './modules/Listener';
import Buttons from './controls/Buttons';

/**
 * Main entry point for the webview
 */

Listener.bindListener();
Globals.initialize(Globals.vscode);
Keyboard.bindKeys();
Buttons.bindButtons();
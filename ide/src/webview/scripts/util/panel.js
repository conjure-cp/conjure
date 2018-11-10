let colours = require('./colours.js');

module.exports = jsPanel.create({
    theme: colours.bgColour + ' filled',
    headerTitle: 'my panel #1',
    position: 'right-top 0 58',
    contentSize: {
        // width: 450,
        width: () => {
            return $(document).width() / 3;
        },
        height: () => {
            return $(document).height() * 0.9;
        }
    },
    content: `
    <div id="pane"> </div>`
});
var express = require('express')
var app = express()

var levels = [{ boxes: [{x: 5, y: 3, color: "red"},
			{x: 1, y: 2, color: "blue"}],
		goals: [{x: 5, y: 6, color: "red"}],
		walls: [{x: 7, y: 3}],
		dimensions: {width: 10, height: 10},
	      },
	      { boxes: [{x: 1, y: 3, color: "green"}],
		goals: [{x: 1, y: 1, color: "green"}],
		walls: [{x: 7, y: 3}],
		dimensions: {width: 10, height: 12},
	      },
              { boxes: [{x: 1, y: 3, color: "blue"}],
		goals: [{x: 0, y: 1, color: "blue"}],
		walls: [{x: 2, y: 3}, {x: 3, y: 3}],
		dimensions: {width: 5, height: 4},
	      }
	     ];

app.get('/levels', function (req, res) {
    var index = parseInt(req.query.id);
    res.send(JSON.stringify(levels[index]));
})

app.use(express.static('public'))

app.listen(3007)

function bindGestures(element, elmGame) {
    var mc = new Hammer(element);
    mc.get('pan').set({ direction: Hammer.DIRECTION_ALL })

    mc.on('panleft panright panup pandown', function(ev) {
        var dir = {"dx" : 0, "dy" : 0};
        switch(ev.type) {
        case 'panleft':
            dir.dx = -1;
            break;
        case 'panright':
            dir.dx = 1;
            break;
        case 'panup':
            dir.dy = 1;
            break;
        case 'pandown':
            dir.dy = -1;
            break;
        }
        elmGame.ports.panDirection.send(dir);
    });
}

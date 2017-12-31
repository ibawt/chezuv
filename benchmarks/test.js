const http = require('http');

for(i = 0 ; i < 1000 ; i++) {
    http.get('http://localhost:8080', (res) => {
        console.log(res.statusCode);
        console.log(res.headers);
        res.setEncoding('utf8');
        let rawData = '';
        res.on('data', (chunk) => { rawData += chunk; });
        res.on('end', () => {
            try {
                console.log(rawData);
            } catch (e) {
                console.error(e.message);
            }
        });
    });
}

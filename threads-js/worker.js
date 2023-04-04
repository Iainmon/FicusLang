const { Worker, isMainThread, workerData } = require('worker_threads');


const workerPool = new Map();

function register(name,fn) {
    const workerData = {
        name: name,
        fn: fn,
    }
    workerPool.set(name,);
}

function spawn(name,param = {}) {
    return new Promise((resolve, reject) => {
        const worker = new Worker(__filename, {
            workerData: param
        });
        worker.on('message', resolve);
        worker.on('error', reject);
        worker.on('exit', (code) => {
            if (code !== 0) {
                reject(new Error(`Worker stopped with exit code ${code}`));
            } else {
                resolve(code);
            }
        });
    });
}

module.exports.worker = worker;
module.exports.workerPool = workerPool;
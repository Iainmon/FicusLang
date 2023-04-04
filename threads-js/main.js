// const { Worker, isMainThread, workerData } = require('worker_threads');



// function worker(param = {}) {
//     return new Promise((resolve, reject) => {
//         const worker = new Worker(__filename, {
//             workerData: param
//         });
//         worker.on('message', resolve);
//         worker.on('error', reject);
//         worker.on('exit', (code) => {
//             if (code !== 0) {
//                 reject(new Error(`Worker stopped with exit code ${code}`));
//             } else {
//                 resolve(code);
//             }
//         });
//     });
// }


// const main = async () => {

//     let x = 1;

//     if (isMainThread) {
//         const worker = new Worker(__filename, { workerData : {
//             name: 'John',
//             task: x
//         }});
//         console.log(x);
//     } else {
//         console.log('Hello from worker thread!');
//         console.log(workerData);
//         workerData.x = 2;
//     }

// }

// main();



import { held } from './async_exec.mjs';

const {resolve, reject, promise} = held()
promise.then(x => console.log(x));
console.debug(promise);
resolve(1);


const p = new Promise((resolveOuter) => {
    resolveOuter(
        new Promise((resolveInner) => {
            setTimeout(() => resolveInner('hi'), 1000);
        }),
    );
});
console.log(p)
const x = await p;

console.log(x);

console.log(x)


// -- type Promise a = P (a -> IO ()) (a -> IO ())
// -- then :: Promise a -> (a -> b) -> Promise b
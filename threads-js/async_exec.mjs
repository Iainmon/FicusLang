

import { scheduler } from 'timers/promises';

// await scheduler.wait(1000); // Wait one second before continuing

// scheduler.wait(1000).then(() => console.log('Hello, world!')); // Wait one second before continuing

// import http from 'http';
// const asyncLocalStorage = new AsyncLocalStorage();

// import { AsyncLocalStorage } from 'async_hooks';


// function logWithId(msg) {
//   const id = asyncLocalStorage.getStore();
//   console.log(`${id !== undefined ? id : '-'}:`, msg);
// }

// let idSeq = 0;
// http.createServer((req, res) => {
//     asyncLocalStorage.run(idSeq++, () => {
//             logWithId('start');
//             // Imagine any chain of async operations here
//             setImmediate(() => {
//             logWithId('finish');
//             res.end();
//         });
//     });
// }).listen(8080);

// http.get('http://localhost:8080');
// http.get('http://localhost:8080');



// export function 


export function held() {
    const hp = {
      resolve: null,
      reject: null,
      promise: null
    };
    const fn = function (res, rej) {
      hp.resolve = res;
      hp.reject = rej;
    }
    hp.promise = new Promise(fn.bind(hp));
    hp.fn = fn;
    return hp;
}


// class LazyPromise {
//     constructor(callback) {
//         const { resolve, reject, promise, fn } = held();
//         promise.then((resolve,reject) => callback(resolve,reject));
//         this.promise = promise;
//     }
//     then() {

//     }
// }

import * as util from 'util';

const promisify = module => {
    return new Proxy(module, {
        get: (moduleName, functionName) => {
            if (functionName in moduleName) {
                if (moduleName[functionName] instanceof Function) {
                    return util.promisify(moduleName[functionName]);
                }
            }
            return moduleName[functionName];
        }
    });
};

function LazyPromise (callback) {
    const ref = { res: null };
    this.promise = new Promise((res,rej) => {
        ref.res = res;
        ref.rej = rej;
    });
    this.ref = ref;
    this.callback = callback;

    async function go(x) {
        const z = this.then(a => a).promise;
        this.callback(this.ref.res,this.ref.rej);
        return await z;
    }
    function then(fn) {
        this.promise = this.promise.then(fn);
        return this;
    }
    this.go = go.bind(this);
    this.then = then.bind(this);

    return this;
}


const lp = new LazyPromise((resolve, reject) => {
    setTimeout(() => resolve(1), 1000);
});
lp.then(x => x + x).then(x => console.log(x));
lp.go().then(x => console.log(x))

const k = new Promise((resolve, reject) => {
    setTimeout(() => resolve(1), 1000);
    return 9;
});
console.info(k,Object.entries(k),typeof(k),k.Object);
console.log(await k);

// data Promise v e = P ((v -> ()))




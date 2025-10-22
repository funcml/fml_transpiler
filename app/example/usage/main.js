const { WASI } = require('node:wasi');
const { readFile } = require('node:fs/promises');
const { argv, env } = require('node:process');
const { join } = require('node:path');

(async () => {
    const projectRoot = process.cwd();
    const wasi = new WASI({
        version: "preview1",
        args: argv,
        env,
        preopens: {
            [projectRoot]: projectRoot,
        },
    });

    const wasmBytes = await readFile(join(process.cwd(), 'bin/Main.wasm'));

    const { instance } = await WebAssembly.instantiate(wasmBytes, {
        wasi_snapshot_preview1: wasi.wasiImport,
    });

    const { memory, hsMalloc, compileFMLtoJS, Main_init__fexports } = instance.exports;

    // 👇 Initialize Haskell runtime
    if (Main_init__fexports) Main_init__fexports();

    function writeCString(str) {
        const bytes = new TextEncoder().encode(str + '\0');
        const ptr = hsMalloc(bytes.length);
        new Uint8Array(memory.buffer, ptr, bytes.length).set(bytes);
        return ptr;
    }

    function readCString(ptr) {
        const bytes = new Uint8Array(memory.buffer);
        let str = '';
        for (let i = ptr; bytes[i] !== 0; i++) str += String.fromCharCode(bytes[i]);
        return str;
    }

    const input = 'Counter => (div a="1" b="2" $ h1 $ "lorem ipsum")';
    const inputPtr = writeCString(input);
    const outputPtr = compileFMLtoJS(inputPtr);
    const output = readCString(outputPtr);

    console.log("Output:", output);
})();

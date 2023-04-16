type KeyTypes = {
    readonly true: 'a';
    readonly false: 'b';
};

type ObjTypes = {
    readonly [index in `${boolean}`]: {
        readonly [key in KeyTypes[index]]: string;
    }
}

type A = ObjTypes['true'];
type B = ObjTypes['false'];

function value<T extends boolean>(useA: T): ObjTypes[`${T}`] {
    return {
        true: { a: 'A' },
        false: { b: 'B' },
    }[`${useA}`];
}

function key<T extends boolean>(useA: T): KeyTypes[`${T}`] {
    return ({
        true: 'a',
        false: 'b',
    } as const)[`${useA}`];
}

function test<T extends boolean>(useA: T): string {
    return value(useA)[key(useA)];
}

console.log(test(true));
console.log(test(false));

export { };

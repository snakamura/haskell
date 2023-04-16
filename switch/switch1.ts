type A = {
    readonly a: string;
};

type B = {
    readonly b: string;
};

function value(useA: boolean): A | B {
    if (useA) {
        return { a: "A" };
    } else {
        return { b: "B" };
    }
}

function key(useA: boolean): keyof A | keyof B {
    if (useA) {
        return 'a';
    } else {
        return 'b';
    }
}

function test(useA: boolean): string {
    return value(useA)[key(useA)];
}

export { };

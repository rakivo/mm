import os
import subprocess as sp

MASM_EXTENSION = '.masm'
MASM_BIN_EXTENSION = '.bin'
EXPECTED_EXTENSION = '.txt'

BUILD_DIR = 'build'
EXPECTED_DIR = 'expected'

MASM_DIR = 'masm'
DEMASM_DIR = 'demasm'

MASM = os.path.join(BUILD_DIR, MASM_DIR)
DEMASM = os.path.join(BUILD_DIR, DEMASM_DIR)

LOG_CMD   = '[CMD]'
LOG_INFO  = '[INFO]'
LOG_PANIC = '[PANIC]'
LOG_ERROR = '[ERROR]'

def cmd(args: list[str]) -> str:
    print(LOG_CMD, *args)
    result = sp.run(' '.join(args), text=True, shell=True, capture_output=True)

    if result.returncode != 0:
        print(LOG_PANIC, f'Process exited abnormally with code {result.returncode}')
        print(LOG_PANIC, result.stderr, end='')
        exit(1)

    return result.stdout

def build_mm():
    cmd(['make'])

def build_examples():
    cmd(['make examples'])

def test_examples():
    files = os.listdir(MASM_DIR)
    for f in files:
        name, ext = os.path.splitext(f)
        if ext != MASM_EXTENSION: continue

        masmf = os.path.join(MASM_DIR, name + MASM_EXTENSION)
        binf = os.path.join(BUILD_DIR, name + MASM_BIN_EXTENSION)

        cmd([MASM, masmf, '-o', binf])
        got_out = cmd([DEMASM, binf])

        with open(os.path.join(EXPECTED_DIR, name + EXPECTED_EXTENSION), 'r') as expected_out:
            got = ' '.join([line.strip() for line in got_out.split('\n')[:-2]])
            expected = ' '.join([line.strip() for line in expected_out.read().split('\n')])
            if got.strip() == expected.strip():
                print(f'{name}: OK')
            else:
                print(f'{name}: FAILED')
                print(f'Got: {got}, expected: {expected}')

if __name__ == '__main__':
    os.makedirs(EXPECTED_DIR, exist_ok=True)

    build_mm()
    build_examples()
    test_examples()

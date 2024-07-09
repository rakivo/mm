import os
import argparse
import subprocess as sp

parser = argparse.ArgumentParser()
parser.add_argument('-r', '--record', action='store_true', help='Record expected outputs')
args = parser.parse_args()

MASM_EXTENSION = '.masm'
MASM_BIN_EXTENSION = '.bin'
EXPECTED_EXTENSION = '.txt'

BUILD_DIR = 'build'
EXPECTED_DIR = 'expected'

MASM_DIR = 'masm'
DEMASM_DIR = 'demasm'

MASM = os.path.join('.', BUILD_DIR, MASM_DIR)
DEMASM = os.path.join('.', BUILD_DIR, DEMASM_DIR)

LOG_CMD   = '[CMD]'
LOG_INFO  = '[INFO]'
LOG_PANIC = '[PANIC]'
LOG_ERROR = '[ERROR]'

SEPARATOR = '---------------------------'

FILES = os.listdir(MASM_DIR)

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

def get_masm_bin_expected(name: str) -> (str, str):
    return os.path.join(MASM_DIR, name + MASM_EXTENSION), os.path.join(BUILD_DIR, name + MASM_BIN_EXTENSION), os.path.join(EXPECTED_DIR, name + EXPECTED_EXTENSION)

def record_examples():
    print(SEPARATOR)
    print(LOG_INFO, 'Recording examples')
    print(SEPARATOR)

    for f in FILES:
        name, ext = os.path.splitext(f)
        if ext != MASM_EXTENSION: continue

        masmf, binf, expected_path = get_masm_bin_expected(name)
        cmd([MASM, masmf, '-o', binf])

        output = cmd([DEMASM, binf])
        with open(expected_path, 'w') as expected:
            print(SEPARATOR)
            print(f'Writing to: {expected_path}..')
            print(SEPARATOR)
            expected.write('\n'.join(output.split('\n')[:-2]))

def test_examples():
    if not args.record: print(SEPARATOR)
    print(LOG_INFO, 'Testing examples')
    print(SEPARATOR)

    for f in FILES:
        name, ext = os.path.splitext(f)
        if ext != MASM_EXTENSION: continue

        masmf, binf, expected_path = get_masm_bin_expected(name)

        cmd([MASM, masmf, '-o', binf])
        got_out = cmd([DEMASM, binf])

        with open(expected_path, 'r') as expected:
            got = ' '.join([line.strip() for line in got_out.split('\n')[:-2]])
            expected = ' '.join([line.strip() for line in expected.read().split('\n')])

            print(SEPARATOR)
            print(f'Comparing output with: {expected_path}')
            if got.strip() == expected.strip():
                print(f'{name} test: OK')
            else:
                print(f'{name}: FAILED')
                print(f'Got: {got}\nExpected: {expected}')
            print(SEPARATOR)

if __name__ == '__main__':
    os.makedirs(EXPECTED_DIR, exist_ok=True)

    build_mm()
    build_examples()

    if args.record:
        record_examples()

    test_examples()

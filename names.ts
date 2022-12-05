const code0 = '0'.charCodeAt(0);
const code9 = '9'.charCodeAt(0);
const codea = 'a'.charCodeAt(0);
const codez = 'z'.charCodeAt(0);
const code_ = '_'.charCodeAt(0);


const tableSize = 0x100; // must be a power of 2
const lsb = (n: number) => n % 0xff;
const msb = (n: number) => Math.floor(n / 0xff) % 0xff;

const unused = 0xff; // hi byte cannot be 0xff
type TableEntry = [string, number];
const table: TableEntry[] = Array(tableSize).fill(null);
const buckets: number[] = Array(tableSize).fill(unused);


const charToBase32 = (code: number) => {
    if (code >= code0 && code <= code9) return code - code0 + 0;
    if (code >= codea && code <= codez) return code - codea + 10;
    if (code === code0) return 36;
    return 0
}

const hash = (s: string) => {
    let sum = 0;
    const s1 = s.toLowerCase();
    for (let i = 0; i < s1.length; i++) {
        const code = s1.charCodeAt(i);
        const b32 = charToBase32(code);
        sum = sum * 43 + b32;
    }
    return sum;
}

const updateEntry = (key: string) => {
    console.log('update',key);
    const h = hash(key);
    const lo = lsb(h);
    let lo1 = lo;
    while (buckets[lo1] !== unused) {
        lo1 = lsb(lo1 + 1);
        if (lo === lo1) {
            console.log(key,' rejected');
            return unused
        };
    };
    if (lo1 !== lo) console.log("missed: ",lo1-lo);
    const hi = msb(h);
    buckets[lo] = hi;
    table[lo] = [key, lo];
    return lo;
}

const tableInit = (wordList: string[]) => {
    for (const w of wordList) {
        const index = updateEntry(w);
    }
}

const tableLookup = (s: string) => {
    const h = hash(s);
    let lo = lsb(h);
    const hi = msb(h);
    if (buckets[lo] === unused) return unused;
    let lo1 = lo;
    while (buckets[lo1] !== hi) {
        lo1 = lsb(lo1 + 1);
        if (lo === lo1 ) return unused;
    }
    return table[lo];
}

const words0 = ["AAH", "ABS", "ACE", "ACK", "ACT", "ADD", "ADO", "ADS", "AFT", "AGE", "AHA", "AHH", "AID", "AIL", "AIM", "AIR", "AKA", "ALE", "ALL", "ALT", "AMP", "AND", "ANT", "ANY", "APE", "APP", "APT", "ARC", "ARE", "ARK", "ARM", "ART", "ASH", "ASK", "ASP", "ASS", "ATE", "AVE", "AWE", "AWK", "AWL", "AWW", "AXE", "AYE", "BAD", "BAG", "BAH", "BAM", "BAN", "BAP", "BAR", "BAT", "BAY", "BBQ", "BED", "BEE", "BEG", "BET", "BIB", "BID", "BIG", "BIN", "BIO", "BIT", "BIZ", "BOB", "BOD", "BOG", "BON", "BOO", "BOP", "BOT", "BOW", "BOX", "BOY", "BRA", "BRO", "BUB", "BUD", "BUG", "BUM", "BUN", "BUR", "BUS", "BUT", "BUY", "BYE", "CAB", "CAD", "CAM", "CAN", "CAP", "CAR", "CAT", "CAY", "COB", "COD", "COG", "COM", "CON", "COO", "COP", "COT", "COW", "COY", "CRY", "CUB", "CUD", "CUE", "CUL", "CUP", "CUT", "DAB", "DAD", "DAM", "DAY", "DEN", "DEW", "DIB", "DID", "DIE", "DIG", "DIM", "DIN", "DIP", "DIR", "DIS", "DOC", "DOE", "DOG", "DOH", "DON", "DOT", "DRY", "DUB", "DUD", "DUE", "DUG", "DUH", "DUO", "DYE", "EAR", "EAT", "EBB", "EEK", "EEL", "EGG", "EGO", "EKE", "ELF", "ELK", "ELM", "EMU", "END", "EON", "ERA", "ERE", "ERR", "EVE", "EWE", "EWW", "EYE", "FAB", "FAD", "FAN", "FAR", "FAT", "FAX", "FED", "FEE", "FEW", "FIB", "FIG", "FIN", "FIR", "FIT", "FIX", "FLU", "FLY", "FOG", "FOR", "FOX", "FRO", "FRY", "FUN", "FUR", "GAB", "GAG", "GAH", "GAL", "GAP", "GAS", "GAY", "GEE", "GEL", "GEM", "GET", "GIG", "GIN", "GNU", "GOB", "GOD", "GOO", "GOT", "GUM", "GUN", "GUT", "GUY", "GYM", "HAD", "HAG", "HAM", "HAS", "HAT", "HAW", "HAY", "HEE", "HEH", "HEM", "HEN", "HER", "HES", "HEW", "HEX", "HEY", "HID", "HIM", "HIP", "HIS", "HIT", "HMM", "HOG", "HOO", "HOP", "HOT", "HOW", "HUB", "HUE", "HUG", "HUH", "HUM", "HUN", "HUT", "ICE", "ICK", "ICY", "IDE", "ILK", "ILL", "IMP", "INK", "INN", "INS", "ION", "IRE", "IRK", "ITS", "IVE", "IVY", "JAB", "JAG", "JAM", "JAR", "JAW", "JAY", "JET", "JIB", "JIG", "JOB", "JOE", "JOG", "JOT", "JOY", "JUG", "JUT", "KEG", "KEY", "KID", "KIN", "KIT", "KOI", "LAB", "LAD", "LAG", "LAM", "LAP", "LAW", "LAX", "LAY", "LED", "LEG", "LET", "LID", "LIE", "LIL", "LIP", "LIT", "LOB", "LOG", "LOL", "LOP", "LOT", "LOW", "LUG", "LUX", "LYE", "MAD", "MAG", "MAN", "MAP", "MAR", "MAT", "MAV", "MAX", "MAY", "MED", "MEH", "MET", "MIC", "MID", "MIN", "MIT", "MIX", "MOB", "MOD", "MOM", "MOO", "MOP", "MOW", "MUD", "MUG", "MUM", "NAB", "NAG", "NAH", "NAP", "NAW", "NAY", "NEG", "NET", "NEW", "NIL", "NIP", "NIT", "NIX", "NOD", "NOG", "NOR", "NOT", "NOW", "NUB", "NUN", "NUT", "OAF", "OAK", "OAR", "OAT", "ODD", "ODE", "OFF", "OFT", "OIL", "OLD", "OLE", "ONE", "OOF", "OPS", "OPT", "ORB", "ORC", "ORE", "ORG", "OUR", "OUT", "OWE", "OWL", "OWN", "PAD", "PAL", "PAN", "PAR", "PAT", "PAW", "PAY", "PEA", "PEG", "PEN", "PEP", "PER", "PET", "PEW", "PIC", "PIE", "PIG", "PIN", "PIP", "PIT", "PLY", "POD", "POO", "POP", "POT", "POW", "PRO", "PRY", "PUB", "PUG", "PUN", "PUP", "PUT", "QUO", "RAD", "RAG", "RAH", "RAM", "RAN", "RAP", "RAT", "RAW", "RAY", "RED", "REF", "REP", "RIB", "RID", "RIG", "RIM", "RIP", "ROB", "ROD", "ROT", "ROW", "RUB", "RUE", "RUG", "RUM", "RUN", "RUT", "RYE", "SAD", "SAG", "SAP", "SAT", "SAW", "SAX", "SAY", "SEA", "SEE", "SET", "SEW", "SEX", "SHE", "SHH", "SHY", "SIM", "SIN", "SIP", "SIR", "SIS", "SIT", "SIX", "SKA", "SKI", "SKY", "SLY", "SOB", "SOD", "SON", "SOS", "SOT", "SOW", "SOY", "SPA", "SPY", "SUB", "SUE", "SUM", "SUN", "SUP", "TAB", "TAC", "TAD", "TAG", "TAN", "TAP", "TAR", "TAT", "TAX", "TEA", "TEE", "TEN", "THE", "THY", "TIC", "TIE", "TIL", "TIN", "TIP", "TIS", "TIT", "TOE", "TON", "TOO", "TOP", "TOT", "TOW", "TOY", "TRY", "TUB", "TUG", "TUX", "TVS", "TWO", "UFO", "UGH", "UHH", "UHM", "UMM", "UMP", "URN", "USE", "UZI", "VAN", "VAT", "VEG", "VET", "VEX", "VIA", "VID", "VIE", "VIP", "VOW", "WAD", "WAG", "WAR", "WAS", "WAX", "WAY", "WED", "WEE", "WET", "WHO", "WHY", "WIG", "WIN", "WIT", "WIZ", "WOE", "WON", "WOO", "WOW", "WRY", "WTF", "YAH", "YAK", "YAM", "YAP", "YAW", "YAY", "YEA", "YEP", "YES", "YET", "YIP", "YON", "YOU", "YUK", "YUM", "YUP", "ZAG", "ZAP", "ZEN", "ZIG", "ZIP", "ZIT", "ZOO"];

const words = [
    'add',
    'and',
    'bytes',
    'case',
    'def',
    'div',
    'exec',
    'eq',
    'filter',
    'get',
    'gt',
    'in',
    'inv',
    'ife',
    'if',
    'key',
    'let',
    'lt',
    'map',
    'neg',
    'or',
    'print',
    'scan',
    'set',
    'shift',
    'sub',
    'switch',
    'while',
    'words',
    'xor'
];

tableInit(words);
// console.log(JSON.stringify(buckets,null,2));
// console.log(JSON.stringify(table, null, 2));

const entry = tableLookup('words');
console.log(entry)
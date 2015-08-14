#+
# A machine-code generation library for the PDP-8.
#
# Written by Lawrence D'Oliveiro <ldo@geek-central.gen.nz>.
#-

wordbits = 12
pagebits = 7
pageselbits = pagebits + 1 # page offset + 0/cur selector bit

class op :
    # memory-reference instruction opcodes
    AND = 0 # AC := AC & mem
    TAD = 1 # AC := AC + mem
    ISZ = 2 # mem := mem + 1; skip next instr if mem is now 0
    DCA = 3 # mem := AC; AC := 0
    JMS = 4 # mem := PC; PC := mem + 1
    JMP = 5 # PC := mem
#end op
# memory locations [0o10 .. 0o17] are preincremented when indirected

class i :
    # remaining instruction mnemonics
    # group 1 operate microinstructions (may be or'ed together to perform various combinations)
    NOP = 0o7000 # no-op
    IAC = 0o7001 # AC := AC + 1
    RAL = 0o7004 # (L, AC) := rotateleft((L, AC), 1)
    RTL = 0o7006 # (L, AC) := rotateleft((L, AC), 2)
    RAR = 0o7010 # (L, AC) := rotateright((L, AC), 1)
    RTR = 0o7012 # (L, AC) := rotateright((L, AC), 2)
    CML = 0o7020 # L := ~L
    CMA = 0o7040 # AC := ~AC
    CIA = 0o7041 # AC := -AC
    CLL = 0o7100 # L := 0
    STL = 0o7120 # L := 1
    CLA = 0o7200 # AC := 0
    STA = 0o7240 # AC := ~0
    # group 2 operate microinstructions (may be or'ed together to perform various combinations)
    HLT = 0o7402 # halt
    OSR = 0o7404 # AC := AC | SR
    SKP = 0o7410 # skip next instr unconditionally
    SNL = 0o7420 # skip next instr iff L != 0
    SZL = 0o7430 # skip next instr iff L = 0
    SZA = 0o7440 # skip next instr iff AC = 0
    SNA = 0o7450 # skip next instr iff AC != 0
    SMA = 0o7500 # skip next instr iff top bit of AC (sign bit) != 0
    SPA = 0o7510 # skip next instr iff top bit of AC (sign bit) = 0
    CLA2 = 0o7600 # AC := 0

    # IOT instructions
    ION = 0o6001 # interrupts on
    IOF = 0o6002 # interrupts off
    # on interrupt, interrupts are turned off, current PC is saved at 0, and PC is set to 1
    # memory extension control type 183
    CDF = 0o6201 # 62N1 data field register := instr >> 3 & 7
    CIF = 0o6202 # 62N2 instr field register := instr >> 3 & 7 on next JMP or JMS
    RDF = 0o6214 # AC := AC & 0o7707 | data field register << 3
    RIF = 0o6224 # AC := AC & 0o7707 | instr field register << 3
    RIB = 0o6234 # AC := AC & 0o7700 | saved instr field << 3 | saved data field
    RMF = 0o6244 # instr field := saved instr field; data field := saved data field

    # extended arithmetic element (EAE) instructions
    MUY = 0o7405 # multiply: (AC, MQ) := MQ * word following; L := 0
    DVI = 0o7407 # divide: (MQ, AC) = divmod((AC, MQ), word following); L := 0
    NMI = 0o7411
      # normalize: (AC, MQ) left-shifted until top two bits of AC not equal or until
      # value is 0o6000 0o000. SC := number of places shifted; L := 0
    SHL = 0o7413 # shift arithmetic left: (L, AC, MQ) shifted left by following word + 1
    ASR = 0o7415 # shift arithmetic right: (L, AC, MQ) shifted right by following word + 1, copying down sign bit
    LSR = 0o7417 # logical shift right: (L, AC, MQ) shifted right by following word + 1
    MQL = 0o7421 # load multiplier quotient: MQ := AC; AC := 0
    SCA = 0o7441 # step counter load into accumulator: AC := AC | SC
    MQA = 0o7501 # multiplier quotient load into accumulator : AC := AC | MQ
    CLA3 = 0o7601 # AC := 0

    # automatic restart type KR01
    SPL = 0o6102 # skip next instr on power low
    # memory parity type 188 NYI
    # teletype control
    KSF = 0o6031 # skip on keyboard flag
    KCC = 0o6032 # clear keyboard flag and AC
    KRS = 0o6034 # keyboard read buffer static: AC := AC & 0o7400 | keyboard buffer, keyboard flag untouched
    KRB = 0o6036 # keyboard read buffer dynamic: AC := keyboard buffer, keyboard flag cleared
    TSF = 0o6041 # skip on teleprinter flag
    TCF = 0o6042 # clear teleprinter flag
    TPC = 0o6044 # load teleprinter and print: teleprinter := AC & 0o377
    TLS = 0o6046 # load teleprinter sequence: teleprinter flag cleared; teleprinter := AC & 0o377
    # teletype system type LT08 multi-teletype ops:
    # op for lines 1-4 is 0o6400 | (linenr - 1) << 4 | (0 for kb/read, 1 for print/punch) << 3 | (bottom 3 op bits as for main teletype)
    # op for line 5 is 0o6110 + ((0 for kb/read, 1 for print/punch) << 3) | (bottom 3 op bits as for main teletype)
    # high-speed tape reader and control type 750C
    RSF = 0o6011 # skip on reader flag
    RRB = 0o6012 # read reader buffer: AC := AC & 0o7400 | buffer; reader flag cleared
    RFC = 0o6014 # reader fetch character: reader flag cleared, read of next char into buffer initiated, flag will be set when done
    # high-speed paper tape punch and control type 75E
    PSF = 0o6021 # skip on punch flag
    PCF = 0o6022 # clear punch flag
    PPC = 0o6024 # load punch buffer and punch: punch buffer := AC & 0o377, then punched; flag untouched
    PLS = 0o6026 # load punch buffer sequence: punch flag cleared; punch buffer := AC & 0o377; punch initiated, flag will be set when done
    # A/D converter type 189
    ADC = 0o6004 # convert analog to digital: AC := digitized quantity
    # A/D converter type 138E, multiplexer type 139E
    ADSF = 0o6531 # skip on A-D flag
    ADCV = 0o6532 # clear flag, initiate conversion, flag set when done
    ADRB = 0o6534 # AC := last converted value, flag cleared
    ADCC = 0o6541 # multiplexer channel address register CAR := 0
    ADSC = 0o6542 # CAR := AC & 0o077, max of 64 single-ended or 32 differential channels
    ADIC = 0o6544 # CAR := CAR + 1 with wraparound
    # D/A converter type AA01A NYI
    # oscilloscope type 34D NYI
    # precision CRT display type 30N NYI
    # light pen type 370 NYI
    # incremental plotter and control type 350B NYI
    # card reader and control type CR01C NYI
    # card reader and control type 451 NYI
    # card punch control type 450 NYI
    # automatic line printer and control type 645 NYI
    # serial magnetic drum system type 251 NYI
    # DECtape systems NYI
    # automatic magnetic tape control type 57A NYI
    # magnetic tape system type 580 NYI
    # data communication systems type 680 NYI
#end i

class CodeBuffer(object) :
    """overall management of a block of generated code."""

    @staticmethod
    def maxbits(val, n) :
        """checks that integer val can be represented in at most n bits."""
        assert (val & ~((1 << n) - 1)) == 0, "val 0%4o doesn't fit in %d bits" % (val, n)
    #end maxbits

    def maxword(self, val) :
        """checks that integer val can fit in one 12-bit machine word."""
        self.maxbits(val, wordbits)
    #end maxword

    class LabelClass(object) :
        # internal representation of a label within the CodeBuffer.

        def __init__(self, name, parent) :
            self.refs = []
            self.name = name
            self.value = None # to begin with
            self.parent = parent
        #end __init__

        def __cmp__(self, other) :
            # just so it can be used as a dictionary key
            return self.name.__cmp__(other.name)
        #end __cmp__

        def __hash__(self) :
            # just so it can be used as a dictionary key
            return self.name.__hash__()
        #end __hash__

        def resolve(self, value = None) :
            """resolves the label to have the specified value."""
            self.parent.resolve(self, value)
            return self # for convenient chaining of calls
        #end resolve

        def resolved(self) :
            """returns True iff the label has been resolved."""
            return self.value != None
        #end if

        def assert_resolved(self) :
            """asserts that the label has been resolved."""
            if self.value == None :
                raise AssertionError("label \"%s\" not resolved" % self.name)
            #end if
        #end assert_resolved

    #end LabelClass

    class PsectClass(object) :
        # internal representation of a program section within the CodeBuffer. Besides
        # allowing logical grouping of code and data sections, I also provide
        # automatic checking that psects don't run into each other.

        def __init__(self, name, parent) :
            self.name = name
            self.origin = None # to begin with
            self.minaddr = None
            self.maxaddr = None
            self.parent = parent
        #end __init__

        def setorigin(self, neworigin) :
            """updates the origin."""
            if neworigin != None :
                check_overlap = False # to begin with
                if self.minaddr == None or self.minaddr > neworigin :
                    self.minaddr = neworigin
                    check_overlap = True
                #end if
                if self.maxaddr == None or self.maxaddr < neworigin :
                    self.maxaddr = neworigin
                    check_overlap = True
                #end if
                if check_overlap :
                    for otherpsect in self.parent.psects.values() :
                        if otherpsect != self and otherpsect.minaddr != None and neworigin >= otherpsect.minaddr and otherpsect.maxaddr != None and neworigin <= otherpsect.maxaddr :
                            raise AssertionError("psect \"%s\" overlaps \"%s\" at location 0%04o" % (self.name, otherpsect.name, neworigin))
                        #end if
                    #end for
                #end if
            #end if
            self.origin = neworigin
            return self # for convenient chaining of calls
        #end setorigin
    #end PsectClass

    def __init__(self) :
        self.blocks = {} # contiguous sequences of defined memory contents, indexed by start address
        self.labels = {}
        self.psects = {}
        self.psect("") # initial default psect
        self.startaddr = None
    #end __init__

    def label(self, name, resolve_here = False) :
        """defines a label with the specified name, if it doesn't already exist.
        Else returns the existing label with that name."""
        if name not in self.labels :
            self.labels[name] = self.LabelClass(name, self)
        #end if
        if resolve_here :
            self.resolve(self.labels[name], self.curpsect.origin)
        #end if
        return self.labels[name]
    #end label

    def _fixup(self, label, addr, bits) :
        # common internal routine for actually fixing up a label reference.
        assert label.value != None
        if bits == pageselbits :
            # memory-reference instruction operand reference
            oldval = self.e(addr)
            mask = (1 << pagebits) - 1
            if addr & ~mask == label.value & ~mask :
                page = 1
            elif label.value & ~mask == 0 :
                page = 0
            else :
                raise AssertionError \
                  (
                    "label %s: illegal cross-page reference" % label.name
                  )
            #end if
            value = oldval & ~mask | page << pagebits | label.value & mask
        elif bits == wordbits :
            # whole-word reference
            value = label.value
        #end if
        self.d(addr, value)
    #end _fixup

    def refer(self, label, addr, bits) :
        """inserts a reference to the specified label at the specified
        location. Note only supported values for bits are 8 (including
        page indicator) or 12. May be called any number of times before or
        after the label is resolved."""
        addr = self.follow(addr)
        assert bits == pageselbits or bits == wordbits
        if label.value != None :
            # resolve straight away
            self._fixup(label, addr, bits)
        else :
            label.refs.append((addr, bits)) # for later resolution
        #end if
        return self # for convenient chaining of calls
    #end refer

    def resolve(self, label, value = None) :
        """marks the label as resolved to the specified address, or the current
        origin if None. Must be called exactly once per label."""
        assert label.value == None # not already resolved
        if value == None :
            value = self.curpsect.origin
        else :
            value = self.follow(value)
        #end if
        assert value != None
        label.value = value
        for addr, bits in label.refs :
            self._fixup(label, addr, bits)
        #end for
        label.refs = []
        return self # for convenient chaining of calls
    #end resolve

    def follow(self, ref, atloc = None, bits = None) :
        # returns ref if it's an integer, or its value if it's a resolved label.
        # An unresolved label is only allowed if atloc is not None; in which
        # case a dummy value is returned, and a reference to the label is added
        # pointing to address atloc of width bits for fixing up later when the
        # label is resolved.
        if type(ref) == self.LabelClass :
            if ref.value == None and atloc != None :
                self.refer(ref, atloc, bits)
                ref = 0 # dummy value, filled in later
            else :
                ref = ref.value
                assert ref != None, "reference to unresolved label %s" % ref.name
            #end if
        else :
            ref = int(ref)
        #end if
        self.maxword(ref)
        return ref
    #end follow

    def psect(self, name) :
        """sets the current program section to the one with the specified name,
        creating it if it doesn't already exist."""
        if name not in self.psects :
            self.psects[name] = self.PsectClass(name, self)
        #end if
        self.curpsect = self.psects[name]
        return self # for convenient chaining of calls
    #end psect

    def e(self, addr) :
        """returns the value at location addr, or 0 if not yet set."""
        addr = self.follow(addr)
        if len(self.blocks) == 1 : # assume just one 4kiW block for all of memory
            val = self.blocks[0][addr]
        else :
            val = 0
        #end if
        return val
    #end e

    def d(self, addr, value) :
        """deposits value into location addr. value may be an unresolved label."""
        if len(self.blocks) == 0 :
            self.blocks[0] = [0] * (1 << wordbits)
              # can't be bothered scrimping on memory, just allocate
              # one 4kiW block
        #end if
        addr = self.follow(addr)
        self.blocks[0][addr] = self.follow(value, addr, wordbits)
        self.lastaddr = addr
        return self # for convenient chaining of calls
    #end d

    def org(self, addr) :
        """sets the origin for defining subsequent consecutive memory contents."""
        self.curpsect.setorigin(self.follow(addr))
        self.lastaddr = self.curpsect.origin
        return self # for convenient chaining of calls
    #end org

    def w(self, value) :
        """deposits value into the current origin and advances it by 1.
        value may be an unresolved label."""
        assert self.curpsect.origin != None, "origin not set"
        self.d(self.curpsect.origin, value)
        self.curpsect.setorigin((self.curpsect.origin + 1) % (1 << wordbits))
        return self # for convenient chaining of calls
    #end w

    def ws(self, values) :
        """deposits a sequence of values into memory starting at the current origin.
        Unresolved labels are allowed."""
        if type(values) == str :
            f = ord
        else : # assume sequence of integers
            f = int
        #end if
        for value in values :
            self.w(f(value))
        #end for
    #end ws

    def mi(self, opc, ind, addr) :
        """generates a memory-reference instruction at the current origin,
        referencing the specified address, which may be an unresolved label."""
        self.maxbits(opc, 3) # assuming it's in [0 .. 5]!
        mask = (1 << pagebits) - 1
        addr = self.follow(addr, self.curpsect.origin, pageselbits)
        if self.curpsect.origin & ~mask == addr & ~mask :
            page = 1
        elif addr & ~mask == 0 :
            page = 0
        else :
            raise AssertionError("illegal cross-page reference")
        #end if
        return self.w(opc << 9 | (0, 1)[ind] << 8 | page << 7 | addr & mask)
    #end mi

    def oi(self, instr) :
        """generates a non-memory-reference instruction at the current origin,
        checking for conflicting bit settings for operate microinstructions."""
        if instr & 0o7000 == 0o7000 :
            if instr & 0o0400 == 0 :
                # group 1
                assert instr & 0o0015 in (0o0000, 0o0001, 0o0004, 0o0010), "illegal combination of group 1 ops"
                  # only rotate in one direction at a time, can't rotate and increment together
            else :
                # group 2
                assert instr & 0o0001 == 0, "illegal group 2 op"
                  # everything else valid?
            #end if
        elif instr & 0o7000 != 0o6000 :
            raise AssertionError("not an IOT or operate instruction")
        #end if
        return self.w(instr)
    #end oi

    def start(self, startaddr) :
        """sets the start-address of the program. This is purely informational
        as far as ths CodeBuffer class is concerned."""
        assert self.startaddr == None
        self.startaddr = self.follow(startaddr)
    #end start

    def done(self) :
        """called at completion of code generation, prior to output of code. Currently
        just checks that all labels are properly resolved."""
        for label in self.labels.values() :
            label.assert_resolved()
        #end for
        return self # for convenient chaining of calls
    #end done

    def dump(self, start = None, end = None) :
        """generator which yields a sequence of (addr, value) pairs for nonzero values
        in order of increasing address over the specified range."""
        for base in sorted(self.blocks.keys()) :
            block = self.blocks[base]
            for offset in range(0, len(block)) :
                addr = base + offset
                value = block[offset]
                if (start == None or addr >= start) and (end == None or addr < end) and value != 0 :
                    yield (addr, value)
                #end if
            #end for
        #end for
        raise StopIteration
    #end dump

#end CodeBuffer

def dump_simh(buf, out) :
    """dumps the contents of CodeBuffer object buf to out as a sequence of
    SIMH memory-deposit commands."""
    for addr, value in buf.dump(0, 0o10000) :
        out.write("d %4o %4o\n" % (addr, value))
    #end for
    if buf.startaddr != None :
        out.write("g %o\n" % buf.startaddr)
    #end if
#end dump_simh

def dump_rim(buf, out, leader = None, trailer = None) :
    """dumps the contents of CodeBuffer object buf to out as in a format that can
    be read by the RIM loader. leader and trailer specify respectively the number
    of leader and trailer bytes to output (leave as None to get the default)."""

    def write_rim(addr, value) :
        # writes out an address-value pair in RIM format.
        out.write \
          (
                chr(0x40 | addr >> 6 & 0o77)
            +
                chr(addr & 0o77)
            +
                chr(value >> 6 & 0o77)
            +
                chr(value & 0o77)
          )
    #end write_rim

#begin dump_rim
    default_leader_trailer = 24 * 10
      # PDP-8 handbook says "about 2 feet" of leader-trailer codes,
      # ECMA-10 says row spacing is 2.54 mm = 0.1 inch
    if leader == None :
        leader = default_leader_trailer
    #end if
    if trailer == None :
        trailer = default_leader_trailer
    #end if
    out.write("\x80" * leader)
    for addr, value in buf.dump(0, 0o10000) :
        write_rim(addr, value)
    #end for
    if buf.startaddr != None :
        write_rim(0o7777, buf.startaddr)
    #end if
    out.write("\x80" * trailer)
#end dump_rim

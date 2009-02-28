#+
# A machine-code generation library for the PDP-8.
#-

wordbits = 12
pagebits = 7

class op :
	# memory-reference instruction opcodes
	AND = 0 # AC := AC & mem
	TAD = 1 # AC := AC + mem
	ISZ = 2 # mem := mem + 1; skip next instr if mem is now 0
	DCA = 3 # mem := AC; AC := 0
	JMS = 4 # mem := PC; PC := mem + 1
	JMP = 5 # PC := mem
#end op
# memory locations [010 .. 017] are preincremented when indirected

class i :
	# remaining instruction mnemonics
	# group 1 operate microinstructions (may be or'ed together to perform various combinations)
	NOP = 07000 # no-op
	IAC = 07001 # AC := AC + 1
	RAL = 07004 # (L, AC) := rotateleft((L, AC), 1)
	RTL = 07006 # (L, AC) := rotateleft((L, AC), 2)
	RAR = 07010 # (L, AC) := rotateright((L, AC), 1)
	RTR = 07012 # (L, AC) := rotateright((L, AC), 2)
	CML = 07020 # L := ~L
	CMA = 07040 # AC := ~AC
	CIA = 07041 # AC := -AC
	CLL = 07100 # L := 0
	STL = 07120 # L := 1
	CLA = 07200 # AC := 0
	STA = 07240 # AC := ~0
	# group 2 operate microinstructions (may be or'ed together to perform various combinations)
	HLT = 07402 # halt
	OSR = 07404 # AC := AC | SR
	SKP = 07410 # skip next instr unconditionally
	SNL = 07420 # skip next instr iff L != 0
	SZL = 07430 # skip next instr iff L = 0
	SZA = 07440 # skip next instr iff AC = 0
	SNA = 07450 # skip next instr iff AC != 0
	SMA = 07500 # skip next instr iff top bit of AC (sign bit) != 0
	SPA = 07510 # skip next instr iff top bit of AC (sign bit) = 0
	CLA2 = 07600 # AC := 0

	# IOT instructions
	ION = 06001 # interrupts on
	IOF = 06002 # interrupts off
	# on interrupt, interrupts are turned off, current PC is saved at 0, and PC is set to 1
	# memory extension control type 183
	CDF = 06201 # 62N1 data field register := instr >> 3 & 7
	CIF = 06202 # 62N2 instr field register := instr >> 3 & 7 on next JMP or JMS
	RDF = 06214 # AC := AC & 07707 | data field register << 3
	RIF = 06224 # AC := AC & 07707 | instr field register << 3
	RIB = 06234 # AC := AC & 07700 | saved instr field << 3 | saved data field
	RMF = 06244 # instr field := saved instr field; data field := saved data field

	# extended arithmetic element (EAE) instructions
	MUY = 07405 # (AC, MQ) := MQ * word following; L := 0
	DVI = 07407 # (MQ, AC) = divmod((AC, MQ), word following); L := 0
	# more TBD

	# automatic restart type KR01
	SPL = 06102 # skip next instr on power low
	# memory parity type 188 NYI
	# teletype control
	KSF = 06031 # skip on keyboard flag
	KCC = 06032 # clear keyboard flag
	KRS = 06034 # keyboard read buffer static: AC := AC & 07400 | keyboard buffer, keyboard flag untouched
	KRB = 06036 # keyboard read buffer dynamic: AC := keyboard buffer, keyboard flag cleared
	TSF = 06041 # skip on teleprinter flag
	TCF = 06042 # clear teleprinter flag
	TPC = 06044 # load teleprinter and print: teleprinter := AC & 0377
	TLS = 06046 # load teleprinter sequence: teleprinter flag cleared; teleprinter := AC & 0377
	# multi-teletype ops NYI
	# high-speed tape reader and control type 750C
	RSF = 06011 # skip on reader flag
	RRB = 06012 # read reader buffer: AC := AC & 07400 | buffer; reader flag cleared
	RFC = 06014 # reader fetch character: reader flag cleared, read of next char into buffer initiated, flag will be set when done
	# high-speed paper tape punch and control type 75E
	PSF = 06021 # skip on punch flag
	PCF = 06022 # clear punch flag
	PPC = 06024 # load punch buffer and punch: punch buffer := AC & 0377, then punched; flag untouched
	PLS = 06026 # load punch buffer sequence: punch flag cleared; punch buffer := AC & 0377; punch initiated, flag will be set when done
	# A/D converter type 189
	ADC = 06004 # convert analog to digital: AC := digitized quantity
	# A/D converter type 138E, multiplexer type 139E
	ADSF = 06531 # skip on A-D flag
	ADCV = 06532 # clear flag, initiate conversion, flag set when done
	ADRB = 06534 # AC := last converted value, flag cleared
	ADCC = 06541 # multiplexer channel address register CAR := 0
	ADSC = 06542 # CAR := AC & 0077, max of 64 single-ended or 32 differential channels
	ADIC = 06544 # CAR := CAR + 1 with wraparound
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

	def __init__(self) :

		def MakeLabelClass(Parent) :
			# creates a new label class bound to the parent CodeBuffer instance.

			class label(object) :
				"""representation of a label within the CodeBuffer."""

				def __init__(self, name) :
					"""name is for informational purposes only, not checked for
					duplicates or anything else."""
					self.refs = []
					self.name = name
					self.value = None # to begin with
					Parent.labels.append(self)
				#end __init__

				def fixup(self, addr, bits) :
					# common internal routine for actually fixing up references
					if bits == pagebits + 1 :
						oldval = Parent.e(addr)
						mask = (1 << pagebits) - 1
						if addr & ~mask == self.value & ~mask :
							page = 1
						elif self.value & ~mask == 0 :
							page = 0
						else :
							raise AssertionError \
							  (
								"label %s: illegal cross-page reference" % self.name
							  )
						#end if
						value = oldval & ~mask | page << pagebits | self.value & mask
					elif bits == wordbits :
						value = self.value
					#end if
					Parent.d(addr, value)
				#end fixup

				def refer(self, addr, bits) :
					"""inserts a reference to the specified label at the specified
					location. Note only supported values for bits are 8 (including
					page indicator) or 12. May be called any number of times before or
					after the label is resolved."""
					Parent.maxword(addr)
					assert bits == pagebits + 1 or bits == wordbits
					if self.value != None :
						# resolve straight away
						self.fixup(addr, bits)
					else :
						self.refs.append((addr, bits)) # for later resolution
					#end if
					return self # for convenient chaining of calls
				#end refer

				def resolve(self, value = None) :
					"""marks the label as resolved to the specified address, or the current
					origin if None. Must be called exactly once per label."""
					assert self.value == None # not already resolved
					if value == None :
						value = Parent.origin
					#end if
					Parent.maxword(value)
					self.value = value
					for addr, bits in self.refs :
						self.fixup(addr, bits)
					#end for
					self.refs = []
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

				def mi(self, op, ind) :
					"""generates a memory-reference instruction in the CodeBuffer
					pointing at the label."""
					Parent.mi(op, ind, 0)
					self.refer(Parent.lastaddr, pagebits + 1)
				#end mi

			#end label

		#begin MakeLabelClass
			return label
		#end MakeLabelClass

	#begin __init__
		self.blocks = {} # contiguous sequences of defined memory contents, indexed by start address
		self.origin = None
		self.labels = []
		self.label = MakeLabelClass(self)
	#end __init__

	def e(self, addr) :
		"""returns the value at location addr, or 0 if not yet set."""
		self.maxword(addr)
		if len(self.blocks) == 1 : # assume just one 4kiW block for all of memory
			val = self.blocks[0][addr]
		else :
			val = 0
		#end if
		return val
	#end e

	def d(self, addr, value) :
		"""deposits value into location addr."""
		self.maxword(addr)
		self.maxword(value)
		if len(self.blocks) == 0 :
			self.blocks[0] = [0] * (1 << wordbits)
			  # can't be bothered scrimping on memory, just allocate
			  # one 4kiW block
		#end if
		self.blocks[0][addr] = value
		self.lastaddr = addr
	#end d

	def org(self, addr) :
		"""sets the origin for defining subsequent consecutive memory contents."""
		self.maxword(addr)
		self.origin = int(addr)
		self.lastaddr = self.origin
	#end if

	def w(self, value) :
		"""deposits value into the current origin and advances it by 1."""
		assert self.origin != None, "origin not set"
		self.d(self.origin, value)
		self.origin = (self.origin + 1) % (1 << wordbits)
	#end w

	def mi(self, op, ind, addr) :
		"""generates a memory-reference instruction at the current origin,
		referencing the specified address."""
		self.maxbits(op, 3) # assuming it's in [0 .. 5]!
		mask = (1 << pagebits) - 1
		if self.origin & ~mask == addr & ~mask :
			page = 1
		elif addr & ~mask == 0 :
			page = 0
		else :
			raise AssertionError("illegal cross-page reference")
		#end if
		self.w(op << 9 | (0, 1)[ind] << 8 | page << 7 | addr & mask)
	#end mi

	def oi(self, instr) :
		"""generates an operate instruction at the current origin, checking
		for conflicting bit settings."""
		assert instr & 07000 == 07000
		if instr & 00400 == 0 :
			# group 1
			assert instr & 00015 in (00000, 00001, 00004, 00010), "illegal combination of group 1 ops"
			  # only rotate in one direction at a time, can't rotate and increment together
		else :
			# group 2
			assert instr & 00001 == 0, "illegal group 2 op"
			  # everything else valid?
		#end if
		self.w(instr)
	#end oi

	def done(self) :
		"""called at completion of code generation, prior to output of code. Currently
		just checks that all labels are properly resolved."""
		for label in self.labels :
			label.assert_resolved()
		#end for
	#end done

	def output(self, start = None, end = None) :
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
	#end output

#end CodeBuffer

if __name__ == "__main__" :
	import sys
	c = CodeBuffer()
	# following sequence assembles the standard RIM loader
	start = 07756
	c.org(start)
	l1 = c.label("l1").resolve()
	c.w(i.KCC)
	l2 = c.label("l2").resolve()
	c.w(i.KSF)
	l2.mi(op.JMP, 0)
	c.w(i.KRB)
	c.oi(i.CLL | i.RTL)
	c.oi(i.RTL)
	c.oi(i.SPA)
	l2.mi(op.JMP, 0)
	c.oi(i.RTL)
	l3 = c.label("l3").resolve()
	c.w(i.KSF)
	l3.mi(op.JMP, 0)
	c.w(i.KRS)
	c.oi(i.SNL)
	temp = c.label("temp")
	temp.mi(op.DCA, 1)
	temp.mi(op.DCA, 0)
	l1.mi(op.JMP, 0)
	temp.resolve()
	c.w(0)
	c.w(0)
	c.done()
	for addr, value in c.output(start, 010000) :
	  # dump as sequence of memory-deposit commands for SIMH
		sys.stdout.write("d %4o %4o\n" % (addr, value))
	#end for
#end if

#+
# A machine-code generation library for the PDP-8.
#-

wordbits = 12
pagebits = 7

opAND = 0 # AC <= AC & mem
opTAD = 1 # AC <= AC + mem
opISZ = 2 # mem <= mem + 1; skip next instr if mem is now 0
opDCA = 3 # mem <= AC; AC <= 0
opJMS = 4 # mem <= PC; PC <= mem + 1
opJMP = 5 # PC <= mem

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
		assert self.origin != None
		self.d(self.origin, value)
		self.origin = (self.origin + 1) % (1 << wordbits)
	#end w

	def mi(self, op, ind, addr) :
		"""generates a memory-reference instruction at the current origin,
		referencing the specified address."""
		assert self.origin != None
		self.maxbits(op, 3)
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

	# more TBD

	def done(self) :
		"""called at completion of code generation, prior to output of code. Currently
		just checks that all labels are properly resolved."""
		for label in self.labels :
			label.assert_resolved()
		#end for
	#end done

	def output(self, start = None, end = None) :
		"""generator which yields a sequence of (addr, value) pairs in order of
		increasing address over the specified range."""
		for base in sorted(self.blocks.keys()) :
			block = self.blocks[base]
			for offset in range(0, len(block)) :
				addr = base + offset
				if (start == None or addr >= start) and (end == None or addr < end) :
					yield (addr, block[offset])
				#end if
			#end for
		#end for
		raise StopIteration
	#end output

#end CodeBuffer

if __name__ == "__main__" :
	import sys
	c = CodeBuffer()
	c.org(07757)
	l1 = c.label("here")
	l2 = c.label("there")
	l1.resolve()
	c.w(06031)
	l1.mi(opJMP, 0)
	l2.mi(opJMP, 0)
	c.w(06032)
	l2.resolve()
	c.w(06036)
	c.done()
	for addr, value in c.output(07756, 010000) :
		sys.stdout.write("d %4o %4o\n" % (addr, value))
	#end for
#end if

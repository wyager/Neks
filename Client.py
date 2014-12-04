#!/usr/bin/env python3
# Exports Server(host, port), which has set(k,v) and get(k) methods.
import msgpack
import socket
import struct

def netFmt(msg):
	length = len(msg)
	formatted = struct.pack(">Q", length) + msg
	return formatted

def netWrite(sock, msg):
	sock.sendall(netFmt(msg))

def recvall(sock, amt):
	data = b''
	while amt != 0:
		new_data = sock.recv(amt)
		if not new_data: return None
		data += new_data
		amt -= len(new_data)
	return data

def netRead(sock):
	length_data = recvall(sock, 8)
	length = struct.unpack(">Q", length_data)[0]
	return recvall(sock, length)

def makeSetMessage(key, value):
	assert(type(key) == bytes)
	assert(type(value) == bytes)
	return msgpack.packb([1, key, value], use_bin_type=True)

def makeGetMessage(key):
	assert(type(key) == bytes)
	return msgpack.packb([0, key], use_bin_type=True)

def parseReply(reply):
	reply = msgpack.unpackb(reply)
	if reply[0] == -1:
		return (reply[1], reply[2]) # Found value for key
	elif reply[1] == -2:
		return (reply[1], None) # Didn't find value for key
	else:
		raise Exception("Invalid response code")

class Server():
	def __init__(self, host, port):
		self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
		self.sock.connect((host, port))
	def set(self, key, value):
		msg = makeSetMessage(key, value)
		netWrite(self.sock, msg)
	def get(self, key):
		msg = makeGetMessage(key)
		netWrite(self.sock, msg)
		reply = parseReply(netRead(self.sock))
		assert(reply[0]==key)
		return reply[1] # None or the value

testKeys = [b"66991fb944", b"afe0c0261a", b"a4242d5dda", b"d10db90845", b"4384ecbfeb", b"a839702a82", b"1ed8680b95", b"0d2189d279", b"f4b0795239", b"a24d4e7e87", b"28e24e1d51", b"9bb0dfbfbd", b"9776bad265", b"89f79a8c71", b"d50de7c1cd", b"167a350f93", b"36f41a6205", b"f5bbd3bc20", b"69a3d20bef", b"33644bede7", b"8744571558", b"cd4ab79d3a", b"8c26e6936c", b"88c1d42e4e", b"f31d532d05", b"a9ad46aea2", b"e9b0aeee64", b"dffc6a25af", b"90952b9ddb", b"04a136756e", b"31ca38445e", b"21c27b172b", b"5c09e01c46", b"9b23b5ef27", b"a9fd5ea170", b"aa1718e735", b"1ce6781a57", b"a927b0584e", b"e7aea00872", b"52223f7078", b"e620de282a", b"1a4c71def8", b"75bd1abc65", b"af93442708", b"2257127db4", b"68ec4b4f7b", b"9b5473f839", b"d453871c0f", b"9657631a3d", b"95503a22b9"]
testValues = [b"5e7a195e90", b"accdfc69c4", b"43be950623", b"afed0a6890", b"0d23711bcf", b"3b3d9b4043", b"139ba09036", b"a54b56630d", b"61a729c150", b"34891805ca", b"d3dc68c9d3", b"e1b4943d72", b"8731015486", b"f8f626c071", b"4262ca1f24", b"3c55632f50", b"d32b8b30ca", b"3311af7221", b"29144d27ea", b"0e0f97257e", b"d6a2e1086b", b"aae1906c17", b"d57f58433f", b"9232138b5e", b"fd1711214f", b"84a66c50ac", b"9b65ffc322", b"d2d447396e", b"6fc6c53265", b"5183bca85b", b"884a5cc1cf", b"7914d452ae", b"6e2a351fd8", b"7fb80954be", b"3c3f1bf0cd", b"112e60a719", b"4917c12e1c", b"9aaf5cc6d1", b"7ccd97a418", b"48c91da08c", b"349524f781", b"7d248047cb", b"9bfec0c3a4", b"c0de587385", b"216dd64a29", b"eac5049f63", b"133a259613", b"843e1f1ee3", b"e9c11331c0", b"48e720933e"]

if __name__ == '__main__':
	server = Server("0.0.0.0",9999)
	for _ in range(1000):
		for (k,v) in zip(testKeys, testValues):
			server.set(k, v)
		for (k,v) in zip(testKeys, testValues):
			assert(server.get(k) == v)
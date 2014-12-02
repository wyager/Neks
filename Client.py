#!/usr/bin/env python3
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

if __name__ == '__main__':
	server = Server("0.0.0.0",9999)
	for i in range(10000):
		server.set(b"hello", b"goodbye")
		assert(server.get(b"hello") == b"goodbye")
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
	b_key = key.encode('utf-8')
	b_value = value.encode('utf-8')
	return msgpack.packb([1, b_key, b_value], use_bin_type=True)

def makeGetMessage(key):
	return msgpack.packb([0, key.encode('utf-8')], use_bin_type=True)

def parseReply(reply):
	reply = msgpack.unpackb(reply)
	if reply[0] == -1:
		return (reply[1], reply[2]) # Found value for key
	elif reply[1] == -2:
		return (reply[1], None) # Didn't find value for key
	else:
		raise Exception("Invalid response code")

if __name__ == '__main__':
	sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	sock.connect(("0.0.0.0",9999))
	msg = makeSetMessage("Jello", "goodbye")
	# for i in range(10000000):
	netWrite(sock, msg)
	msg = makeGetMessage("Jello")
	print(msg)
	for i in range(100000):
		netWrite(sock, msg)
		reply = parseReply(netRead(sock))
		assert(reply == (b'Jello', b'goodbye'))
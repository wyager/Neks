#!/usr/bin/env python3
import msgpack
import socket
import struct

def netFmt(msg):
	length = len(msg)
	formatted = struct.pack(">Qs", length, msg)
	return formatted

def netWrite(msg, sock):
	sock.sendall(netFmt(msg))

def recvall(sock, amt):
	data = b''
	while amt != 0:
		new_data = sock.recv(amt)
		if new_data == None: return None
		data += new_data
		amt -= len(new_data)
	return data

def netRead(sock):
	length_data = recvall(sock, 8)
	length = struct.unpack(">Q", length_data)
	return recvall(sock, length)

def setMsg(key, value):
	

if __name__ == '__main__':
	sock = socket.connect(("0.0.0.0",9999))

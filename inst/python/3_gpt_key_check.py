from openai import OpenAI
import os

def gpt_key_check(key):
	client = OpenAI(api_key=key)
	try:
		client.models.list()
	except:
		return False
	else:
		return True
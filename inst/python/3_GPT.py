from openai import OpenAI
import os

def run_chatGPT(x, y, z):
    # x, the prompt
    # y, the api key
    # z, the model

    # Define the prompt
    prompt = [{"role": "system", "content": x}]
    
    # Set up OpenAI API credentials
    #openai.api_key = y
    
    client = OpenAI(
    	# This is the default and can be omitted
    	api_key= y,
    )

    # Define the parameters for the text generation
    max_tokens = 10000  # Maximum number of tokens to generate
    temperature = 0.2  # Controls the "creativity" of the generated text

    # Generate text based on the prompt and parameters
    #response = openai.ChatCompletion.create(
    #    model = z,
    #    messages = prompt,
    #    max_tokens = max_tokens,
    #    temperature = temperature,
    #)
    
    response = client.chat.completions.create(
        model = z,
        messages = prompt,
        max_tokens = max_tokens,
        temperature = temperature,
    )
    

    # Print the generated text
    final_answer = response.choices[0].message.content
    return final_answer

    # python3 -m pip install openai

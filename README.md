# org-ai [![MELPA](https://melpa.org/packages/org-ai-badge.svg)](https://melpa.org/#/org-ai)

[![org-ai video](doc/org-ai-youtube-thumb-github.png)](https://www.youtube.com/watch?v=fvBDxiFPG6I)

Minor mode for Emacs org-mode that provides access to generative AI models. Currently supported are
- OpenAI API (ChatGPT, DALL-E, other text models), optionally run against Azure API instead of OpenAI
- Stable Diffusion through [stable-diffusion-webui](https://github.com/AUTOMATIC1111/stable-diffusion-webui)

Inside an org-mode buffer you can
- use ChatGPT to generate text, having full control over system and user prompts ([demo](#chatgpt-in-org-mode))
- Speech input and output! Talk with your AI!
- generate images and image variations with a text prompt using Stable Diffusion or DALL-E ([demo 1](#dall-e-in-org-mode), [demo 2](#image-variations))
- org-ai everywhere: Various commands usable outside org-mode for prompting using the selected text or multiple files.

_Note: In order to use the OpenAI API you'll need an [OpenAI account](https://platform.openai.com/) and you need to get an API token. As far as I can tell, the current usage limits for the free tier get you pretty far._

------------------------------

## Table of Contents

- [Demos](#demos)
    - [ChatGPT in org-mode](#chatgpt-in-org-mode)
    - [DALL-E in org-mode](#dall-e-in-org-mode)
    - [Image variations](#image-variations)
- [Features and Usage](#features-and-usage)
    - [`#+begin_ai...#+end_ai` special blocks](#begin_aiend_ai-special-blocks)
        - [Syntax highlighting in ai blocks](#syntax-highlighting-in-ai-blocks)
        - [Block Options](#block-options)
            - [For ChatGPT](#for-chatgpt)
            - [For DALL-E](#for-dall-e)
            - [Other text models](#other-text-models)
    - [Image variation](#image-variation)
    - [Global Commands](#global-commands)
        - [org-ai-on-project](#org-ai-on-project)
    - [Noweb Support](#noweb-support)
- [Installation](#installation)
    - [Melpa](#melpa)
    - [Straight.el](#straightel)
    - [Manual](#manual)
    - [OpenAI API key](#openai-api-key)
        - [Using Azure API instead of OpenAI](#using-azure-api-instead-of-openai)
    - [Setting up speech input / output](#setting-up-speech-input--output)
        - [Whisper](#whisper)
            - [macOS specific steps](#macos-specific-steps)
                - [macOS alternative: Siri dictation](#macos-alternative-siri-dictation)
            - [Windows specific steps](#windows-specific-steps)
        - [espeak / greader](#espeak--greader)
    - [Setting up Stable Diffusion](#setting-up-stable-diffusion)
    - [Using local LLMs with oobabooga/text-generation-webui](#using-local-llms-with-oobaboogatext-generation-webui)
- [FAQ](#faq)
- [Sponsoring](#sponsoring)

## Demos

### ChatGPT in org-mode

```org
#+begin_ai
Is Emacs the greatest editor?
#+end_ai
```

![chat-gpt in org-mode](doc/org-ai-demo-1.gif)

You can continue to type and press `C-c C-c` to create a conversation. `C-g` will interrupt a running request.


### DALL-E in org-mode

Use the `:image` keyword to generate an image. This uses DALL·E-3 by default.

```org
#+begin_ai :image :size 1024x1024
Hyper realistic sci-fi rendering of super complicated technical machine.
#+end_ai
```

![dall-e in org-mode](doc/org-ai-demo-2.gif)

You can use the following keywords to control the image generation:
- `:size <width>x<height>` - the size of the image to generate (default: 1024x1024)
- `:model <model>` - the model to use (default: `"dall-e-3"`)
- `:quality <quality>` - the quality of the image (choices: `hd`, `standard`)
- `:style <style>` - the style to use (choices: `vivid`, `natural`)
- `:n <count> - the number of images to generate (default: 1)

(For more information about those settings see [this OpenAI blog post](https://cookbook.openai.com/articles/what_is_new_with_dalle_3).

You can customize the defaults for those variables with `customize-variable` or by setting them in your config:

```elisp
(setq org-ai-image-model "dall-e-3")
(setq org-ai-image-default-size "1792x1024")
(setq org-ai-image-default-count 2)
(setq org-ai-image-default-style 'vivid)
(setq org-ai-image-default-quality 'hd)
(setq org-ai-image-directory (expand-file-name "org-ai-images/" org-directory))
```


### Image variations

![dall-e image generation in org-mode](doc/org-ai-demo-3.gif)



## Features and Usage
### `#+begin_ai...#+end_ai` special blocks

Similar to org-babel, these blocks demarcate input (and for ChatGPT also output) for the AI model. You can use it for AI chat, text completion and text -> image generation. See [options](#block-options) below for more information.

Create a block like

```org
#+begin_ai
Is Emacs the greatest editor?
#+end_ai
```

and press `C-c C-c`. The Chat input will appear inline and once the response is complete, you can enter your reply and so on. See [the demo](#chatgpt-in-org-mode) below. You can press `C-g` while the ai request is running to cancel it.

You can also modify the _system_ prompt and other parameters used. The system prompt is injected before the user's input and "primes" the model to answer in a certain style. For example you can do:

```org
#+begin_ai :max-tokens 250
[SYS]: Act as if you are a powerful medival king.
[ME]: What will you eat today?
#+end_ai
```

This will result in an API payload like

```json
{
  "messages": [
    {
      "role": "system",
      "content": "Act as if you are a powerful medival king."
    },
    {
      "role": "user",
      "content": "What will you eat today?"
    }
  ],
  "model": "gpt-3.5-turbo",
  "stream": true,
  "max_tokens": 250,
  "temperature": 1.2
}
```

For some prompt ideas see for example [Awesome ChatGPT Prompts](https://github.com/f/awesome-chatgpt-prompts).

When generating images using the `:image` flag, images will appear underneath the ai block inline. Images will be stored (together with their prompt) inside `org-ai-image-directory` which defaults to `~/org/org-ai-images/`.

You can also use speech input to transcribe the input. Press `C-c r` for `org-ai-talk-capture-in-org` to start recording. Note that this will require you to setup [speech recognition](#setting-up-speech-input--output) (see below). Speech output can be enabled with `org-ai-talk-output-enable`.

Inside an `#+begin_ai...#+end_ai` you can modify and select the parts of the chat with these commands:
- Press `C-c <backspace>` (`org-ai-kill-region-at-point`) to remove the chat part under point.
- `org-ai-mark-region-at-point` will mark the region at point.
- `org-ai-mark-last-region` will mark the last chat part.

#### Syntax highlighting in ai blocks

To apply syntax highlighted to your `#+begin_ai ...` blocks just add a language major-mode name after `_ai`. E.g. `#+begin_ai markdown`. For markdown in particular, to then also correctly highlight code in in backticks, you can set `(setq markdown-fontify-code-blocks-natively t)`. Make sure that you also have the [markdown-mode package](https://melpa.org/#/markdown-mode) installed. Thanks @tavisrudd for this trick!

#### Block Options

The `#+begin_ai...#+end_ai` block can take the following options.

##### For ChatGPT
By default, the content of ai blocks are interpreted as messages for ChatGPT. Text following `[ME]:` is associated with the user, text following `[AI]:` is associated as the model's response. Optionally you can start the block with a `[SYS]: <behavior>` input to prime the model (see `org-ai-default-chat-system-prompt` below).

- `:max-tokens number` - number of maximum tokens to generate (default: nil, use OpenAI's default)
- `:temperature number` - temperature of the model (default: 1)
- `:top-p number` - top_p of the model (default: 1)
- `:frequency-penalty number` - frequency penalty of the model (default: 0)
- `:presence-penalty` - presence penalty of the model (default: 0)
- `:sys-everywhere` - repeat the system prompt for every user message (default: nil)

If you have a lot of different threads of conversation regarding the same topic and settings (system prompt, temperature, etc) and you don't want to repeat all the options, you can set org file scope properties or create a org heading with property drawer, such that all `#+begin_ai...#+end_ai` blocks under that heading will inherit the settings.

Examples:
```org
* Emacs (multiple conversations re emacs continue in this subtree)
:PROPERTIES:
:SYS: You are a emacs expert. You can help me by answering my questions. You can also ask me questions to clarify my intention.
:temperature: 0.5
:model: gpt-3.5-turbo
:END:

** Web programming via elisp
#+begin_ai
How to call a REST API and parse its JSON response?
#+end_ai

** Other emacs tasks
#+begin_ai...#+end_ai

* Python (multiple conversations re python continue in this subtree)
:PROPERTIES:
:SYS: You are a python programmer. Respond to the task with detailed step by step instructions and code.
:temperature: 0.1
:model: gpt-4
:END:

** Learning QUIC
#+begin_ai
How to setup a webserver with http3 support?
#+end_ai

** Other python tasks
#+begin_ai...#+end_ai
```

The following custom variables can be used to configure the chat:

- `org-ai-default-chat-model` (default: `"gpt-3.5-turbo"`)
- `org-ai-default-max-tokens` How long the response should be. Currently cannot exceed 4096. If this value is too small an answer might be cut off (default: nil)
- `org-ai-default-chat-system-prompt` How to "prime" the model. This is a prompt that is injected before the user's input. (default: `"You are a helpful assistant inside Emacs."`)
- `org-ai-default-inject-sys-prompt-for-all-messages` Wether to repeat the system prompt for every user message. Sometimes the model "forgets" how it was primed. This can help remind it. (default: `nil`)

##### For DALL-E

When you add an `:image` option to the ai block, the prompt will be used for image generation.

- `:image` - generate an image instead of text
- `:size` - size of the image to generate (default: 256x256, can be 512x512 or 1024x1024)
- `:n` - the number of images to generate (default: 1)

The following custom variables can be used to configure the image generation:
- `org-ai-image-directory` - where to store the generated images (default: `~/org/org-ai-images/`)

##### For Stable Diffusion

Similar to DALL-E but use

```
#+begin_ai :sd-image
<PROMPT>
#+end_ai
```

You can run img2img by labeling your org-mode image with #+name and
referencing it with :image-ref from your org-ai block.

```
#+begin_ai :sd-image :image-ref label1
forest, Gogh style
#+end_ai
```

M-x org-ai-sd-clip guesses the previous image's prompt on org-mode
by the CLIP interrogator and saves it in the kill ring.

M-x org-ai-sd-deepdanbooru guesses the previous image's prompt on
org-mode by the DeepDanbooru interrogator and saves it in the kill
ring.

##### For local models
For requesting completions from a local model served with [oobabooga/text-generation-webui](https://github.com/oobabooga/text-generation-webui), go through the setup steps described [below](#using-local-llms-with-oobaboogatext-generation-webui)

Then start an API server:

``` sh
cd ~/.emacs.d/org-ai/text-generation-webui
conda activate org-ai
python server.py --api --model SOME-MODEL
```

When you add a `:local` key to an org-ai block and request completions with `C-c C-c`, the block will be sent to the local API server instead of the OpenAI API. For example:

```
#+begin_ai :local
...
#+end_ai
```

This will send a request to `org-ai-oobabooga-websocket-url` and stream the response into the org buffer.

##### Other text models

The older completion models can also be prompted by adding the `:completion` option to the ai block.

- `:completion` - instead of using the chatgpt model, use the completion model
- `:model` - which model to use, see https://platform.openai.com/docs/models for a list of models

For the detailed meaning of those parameters see the [OpenAI API documentation](https://platform.openai.com/docs/api-reference/chat).

The following custom variables can be used to configure the text generation:

- `org-ai-default-completion-model` (default: `"text-davinci-003"`)



### Image variation

You can also use an existing image as input to generate more similar looking images. The `org-ai-image-variation` command will prompt for a file path to an image, a size and a count and will then generate as many images and insert links to them inside the current `org-mode` buffer. Images will be stored inside `org-ai-image-directory`. See the [demo](#image-variations) below.

[For more information see the OpenAI documentation](https://platform.openai.com/docs/guides/images/variations). The input image needs to be square and its size needs to be less than 4MB. And you currently need curl available as a command line tool[^1].

[^1]: __Note:__ Currenly the image variation implementation requires a command line curl to be installed. Reason for that is that the OpenAI API expects multipart/form-data requests and the emacs built-in `url-retrieve` does not support that (At least I haven't figured out how). Switching to `request.el` might be a better alternative. If you're interested in contributing, PRs are very welcome!



### Global Commands

`org-ai` can be used outside of `org-mode` buffers as well. When you enable `org-ai-global-mode`, the prefix `C-c M-a` will be bound to a number of commands:

| command                          | keybinding  | description                                                                                                                                                                                                                                                                                                                                                    |
|:---------------------------------|:------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `org-ai-on-region`               | `C-c M-a r` | Ask a question about the selected text or tell the AI to do something with it. The response will be opened in an org-mode buffer so that you can continue the conversation. Setting the variable `org-ai-on-region-file` (e.g. `(setq org-ai-on-region-file (expand-file-name "org-ai-on-region.org" org-directory))`) will associate a file with that buffer. |
| `org-ai-summarize`               | `C-c M-a s` | Summarize the selected text.                                                                                                                                                                                                                                                                                                                                   |
| `org-ai-refactor-code`           | `C-c M-a c` | Tell the AI how to change the selected code, a diff buffer will appear with the changes.                                                                                                                                                                                                                                                                       |
| `org-ai-on-project`              | `C-c M-a p` | Run prompts and modify / refactor multiple files at once. Will use [projectile](https://github.com/bbatsov/projectile) if available, falls back to the current directory if not.                                                                                                                                                                               |
| `org-ai-prompt`                  | `C-c M-a P` | Prompt the user for a text and then print the AI's response in current buffer.                                                                                                                                                                                                                                                                                 |
| `org-ai-switch-chat-model`       | `C-c M-a m` | Interactively change `org-ai-default-chat-model`                                                                                                                                                                                                                                                                                                               |
| `org-ai-open-account-usage-page` | `C-c M-a $` | Opens https://platform.openai.com/account/usage to see how much money you have burned.                                                                                                                                                                                                                                                                         |
| `org-ai-open-request-buffer`     | `C-c M-a !` | Opens the `url` request buffer. If something doesn't work it can be helpful to take a look.                                                                                                                                                                                                                                                                    |
| `org-ai-talk-input-toggle`       | `C-c M-a t` | Generally enable speech input for the different prompt commands.                                                                                                                                                                                                                                                                                               |
| `org-ai-talk-output-toggle`      | `C-c M-a T` | Generally enable speech output.                                                                                                                                                                                                                                                                                                                                |

#### org-ai-on-project

Using the org-ai-on-project buffer allows you to run commands on files in a project, alternatively also just on selected text in those files. You can e.g. select the readme of a project and ask "what is it all about?" or have code explained to you. You can also ask for code changes, which will generate a diff. If you know somehone who thinks only VS Code with Copilot enabled can do that, point them here.

Running the `org-ai-on-project` command will open a separate buffer that allows you to select choose multiple files (and optionally select a sub-region inside a file) and then run a prompt on it.

![org-ai-on-project](doc/org-ai-on-project-buffer.png)

If you deactivate "modify code", the effect is similar to running `org-ai-on-region` just that the file contents all appear in the prompt.

With "modify code" activated, you can ask the AI to modify or refactor the code. By default ("Request diffs") deactivated, we will prompt to generate the new code for all selected files/regions and you can then see a diff per file and decide to apply it or not. With "Request diffs" active, the AI will be asked to directly create a unified diff that can then be applied.


### Noweb Support

Given a named source block
```
#+name: sayhi
#+begin_src shell
echo "Hello there"
#+end_src
```
We can try to reference it by name, but it doesn't work.
```
#+begin_ai
[SYS]: You are a mimic. Whenever I say something, repeat back what I say to you. Say exactly what I said, do not add anything.

[ME]: <<sayhi()>>


[AI]: <<sayhi()>>

[ME]:
#+end_ai
```
With `:noweb yes`

```
#+begin_ai :noweb yes
[SYS]: You are a mimic. Whenever I say something, repeat back what I say to you. Say exactly what I said, do not add anything.

[ME]: <<sayhi()>>


[AI]: Hello there.

[ME]:
#+end_ai
```

You can also trigger noweb expansion with an `org-ai-noweb: yes` heading proprty anywhere in the parent headings (header args takes precedence).

To see what your document will expand to when sent to the api, run `org-ai-expand-block`.

#### Run arbitrary lisp inline

This is a hack but it works really well.

Create a block

```
#+name: identity
#+begin_src emacs-lisp :var x="fill me in"
(format "%s" x)
#+end_src
```

We can invoke it and let noweb parameters (which support lisp) evaluate as code

```
#+begin_ai :noweb yes
Tell me some 3, simple ways to improve this dockerfile

<<identity(x=(quelpa-slurp-file "~/code/ibr-api/Dockerfile"))>>



[AI]: 1. Use a more specific version of Python, such as "python:3.9.6-buster" instead of "python:3.9-buster", to ensure compatibility with future updates.

2. Add a cleanup step after installing poetry to remove any unnecessary files or dependencies, thus reducing the size of the final image.

3. Use multi-stage builds to separate the build environment from the production environment, thus reducing the size of the final image and increasing security. For example, the first stage can be used to install dependencies and build the code, while the second stage can contain only the final artifacts and be used for deployment.

[ME]:
#+end_ai
```


## Installation

### Melpa

org-ai is on Melpa: https://melpa.org/#/org-ai. If you have added Melpa to your package archives with

```elisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
```

you can install it with:

```elisp
(use-package org-ai
  :ensure t
  :commands (org-ai-mode
             org-ai-global-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  (setq org-ai-default-chat-model "gpt-4") ; if you are on the gpt-4 beta:
  (org-ai-install-yasnippets)) ; if you are using yasnippet and want `ai` snippets

```

### Straight.el

```elisp
(straight-use-package
 '(org-ai :type git :host github :repo "rksm/org-ai"
          :local-repo "org-ai"
          :files ("*.el" "README.md" "snippets")))
```

### Manual

Checkout this repository.

```sh
git clone
https://github.com/rksm/org-ai
```

Then, if you use `use-package`:

```elisp
(use-package org-ai
  :ensure t
  :load-path (lambda () "path/to/org-ai"))
  ;; ...rest as above...

```

or just with `require`:

```elisp
(package-install 'websocket)
(add-to-list 'load-path "path/to/org-ai")
(require 'org)
(require 'org-ai)
(add-hook 'org-mode-hook #'org-ai-mode)
(org-ai-global-mode)
(setq org-ai-default-chat-model "gpt-4") ; if you are on the gpt-4 beta:
(org-ai-install-yasnippets) ; if you are using yasnippet and want `ai` snippets
```

### OpenAI API key

You can either directly set your api token in your config:

```elisp
(setq org-ai-openai-api-token "<ENTER YOUR API TOKEN HERE>")

```

Alternatively, `org-ai` supports `auth-source` for retrieving your API key. You can store a secret in the format

```
machine api.openai.com login org-ai password <your-api-key>
```

in your `~/authinfo.gpg` file. If this is present, org-ai will use this mechanism to retrieve the token when a request is made. If you do not want `org-ai` to try to retrieve the key from `auth-source`, you can set `org-ai-use-auth-source` to `nil` before loading `org-ai`.

#### Using Azure API instead of OpenAI

You can switch to Azure by customizing these variables, either interactively with `M-x customize-variable` or by adding them to your config:

``` elisp
(setq org-ai-service 'azure-openai
      org-ai-azure-openai-api-base "https://your-instance.openai.azure.com"
      org-ai-azure-openai-deployment "azure-openai-deployment-name"
      org-ai-azure-openai-api-version "2023-07-01-preview")
```

To store the API credentials, follow the authinfo instructions above but use `org-ai-azure-openai-api-base` as the machine name.

### Setting up speech input / output

#### Whisper

These setup steps are optional. If you don't want to use speech input / output, you can skip this section.

_Note: My personal config for org-ai can be found in [this gist](https://gist.github.com/rksm/04be012be07671cd5e1dc6ec5b077e34). It contains a working whisper setup._

This has been tested on macOS and Linux. Someone with a Windows computer, please test this and let me know what needs to be done to make it work (Thank You!).

The speech input uses [whisper.el](https://github.com/natrys/whisper.el) and `ffmpeg`. You need to clone the repo directly or use [straight.el](https://github.com/radian-software/straight.el) to install it.

1. install ffmpeg (e.g. `brew install ffmpeg` on macOS) or `sudo apt install ffmpeg` on Linux.
2. Clone whisper.el: `git clone https://github.com/natrys/whisper.el path/to/whisper.el`

You should now be able to load it inside Emacs:

```elisp
(use-package whisper
  :load-path "path/to/whisper.el"
  :bind ("M-s-r" . whisper-run))
```

Now also load:

```elisp
(use-package greader :ensure)
(require 'whisper)
(require 'org-ai-talk)

;; macOS speech settings, optional
(setq org-ai-talk-say-words-per-minute 210)
(setq org-ai-talk-say-voice "Karen")
```

##### macOS specific steps

On macOS you will need to do two more things:
1. Allow Emacs to record audio
2. Tell whisper.el what microphone to use

###### 1. Allow Emacs to record audio
You can use the [tccutil helper](https://github.com/DocSystem/tccutil):

```sh
git clone https://github.com/DocSystem/tccutil
cd tccutil
sudo python ./tccutil.py -p /Applications/Emacs.app -e --microphone
```

When you now run `ffmpeg -f avfoundation -i :0 output.mp3` from within an Emacs shell, there should be no `abort trap: 6` error.

(As an alternative to tccutil.py see the method mentioned in [this issue](https://github.com/rksm/org-ai/issues/86).)

###### 2. Tell whisper.el what microphone to use

You can use the output of `ffmpeg -f avfoundation -list_devices true -i ""` to list the audio input devices and then tell whisper.el about it: `(setq whisper--ffmpeg-input-device ":0")`. `:0` is the microphone index, see the output of the command above to use another one.

I've created an emacs helper that let's you select the microphone interactively. See [this gist](https://gist.github.com/rksm/04be012be07671cd5e1dc6ec5b077e34#file-init-org-ai-el-L6).

My full speech enabled config then looks like:

```elisp
(use-package whisper
  :load-path (lambda () (expand-file-name "lisp/other-libs/whisper.el" user-emacs-directory))
  :config
  (setq whisper-model "base"
        whisper-language "en"
        whisper-translate nil)
  (when *is-a-mac*
    (rk/select-default-audio-device "Macbook Pro Microphone")
    (when rk/default-audio-device)
    (setq whisper--ffmpeg-input-device (format ":%s" rk/default-audio-device))))
```

###### macOS alternative: Siri dictation

On macOS, instead of whisper, you can also use the built-in Siri dictation. To enable that, go to `Preferences -> Keyboard -> Dictation`, enable it and set up a shortcut. The default is ctrl-ctrl.

##### Windows specific steps

The way (defun whisper--check-install-and-run) is implemented does not work on Win10 (see https://github.com/rksm/org-ai/issues/66).

A workaround is to install whisper.cpp and model manually and patch:

``` elisp
(defun whisper--check-install-and-run (buffer status)
  (whisper--record-audio))
```

#### espeak / greader

Speech output on non-macOS systems defaults to using the [greader](http://elpa.gnu.org/packages/greader.html) package which uses [espeak](https://espeak.sourceforge.net/) underneath to synthesize speech. You will need to install greader manually (e.g. via `M-x package-install`). From that point on it should "just work". You can test it by selecting some text and calling `M-x org-ai-talk-read-region`.

### Setting up Stable Diffusion

An API for Stable Diffusion can be hosted with the [stable-diffusion-webui](https://github.com/AUTOMATIC1111/stable-diffusion-webui) project. Go through the [install steps for your platform](https://github.com/AUTOMATIC1111/stable-diffusion-webui#installation-and-running), then start an API-only server:

```sh
cd path/to/stable-diffusion-webui
./webui.sh --nowebui
```

This will start a server on http://127.0.0.1:7861 by default. In order to use it with org-ai, you need to set `org-ai-sd-endpoint-base`:

```elisp
(setq org-ai-sd-endpoint-base "http://localhost:7861/sdapi/v1/")
```

If you use a server hosted elsewhere, change that URL accordingly.

### Using local LLMs with oobabooga/text-generation-webui
Since version 0.4 org-ai supports local models served with [oobabooga/text-generation-webui](https://github.com/oobabooga/text-generation-webui). See the [installation instructions](https://github.com/oobabooga/text-generation-webui#installation) to set it up for your system.

Here is a setup walk-through that was tested on Ubuntu 22.04. It assumes [miniconda or Anaconda](https://docs.conda.io/projects/conda/en/stable/user-guide/install/download.html#anaconda-or-miniconda) as well as [git-lfs](https://git-lfs.com/) to be installed.

#### Step 1: Setup conda env and install pytorch

```sh
conda create -n org-ai python=3.10.9
conda activate org-ai
pip3 install torch torchvision torchaudio
```

#### Step 2: Install oobabooga/text-generation-webui

```sh
mkdir -p ~/.emacs.d/org-ai/
cd ~/.emacs.d/org-ai/
git clone https://github.com/oobabooga/text-generation-webui
cd text-generation-webui
pip install -r requirements.txt
```

#### Step 3: Install a language model

oobabooga/text-generation-webui supports [a number of language models](https://github.com/oobabooga/text-generation-webui#downloading-models). Normally, you would install them from [huggingface](https://huggingface.co/models?pipeline_tag=text-generation&sort=downloads). For example, to install the `CodeLlama-7b-Instruct` model:

```sh
cd ~/.emacs.d/org-ai/text-generation-webui/models
git clone git@hf.co:codellama/CodeLlama-7b-Instruct-hf
```

#### Step 4: Start the API server

```sh
cd ~/.emacs.d/org-ai/text-generation-webui
conda activate org-ai
python server.py --api --model CodeLlama-7b-Instruct-hf
```

Depending on your hardware and the model used you might need to adjust the server parameters, e.g. use `--load-in-8bit` to reduce memory usage or `--cpu` if you don't have a suitable GPU.

You should now be able to use the local model with org-ai by adding the `:local` option to the `#+begin_ai` block:

```
#+begin_ai :local
Hello CodeLlama!
#+end_ai
```

## FAQ

### Is this OpenAI specfic?
No, OpenAI is the easiest to setup (you only need an API key) but you can use local models as well. See how to use Stable Diffusion and local LLMs with oobabooga/text-generation-webui above.

### Are there similar projects around?

The gptel package provides an alternative interface to the OpenAI ChatGPT API: https://github.com/karthink/gptel


## Sponsoring

If you find this project useful please consider [sponsoring](https://github.com/sponsors/rksm). Thank you!

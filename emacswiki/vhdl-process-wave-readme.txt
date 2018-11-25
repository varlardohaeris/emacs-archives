This file can be used with saved modelsim wave files. Just invoke
vhdl-process-wave command while in saved buffer, save the buffer,
reload waveform into modelsim. Waveform will look much better now.

For customization have a look at
M-x customize-group whdl-process-wave-general
M-x customize-group whdl-process-wave-project

It is possible to define rules how to convert the waveform on
project basis (delete this signal, put this signal on top, let
signal xyz have radix decimal, make signal zyx purple etc.)

See customization description for more information.

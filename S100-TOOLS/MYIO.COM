1$�	� >�ӫ� �k	� ��0�w�1�?�2���3��4�N�����	� � �k	� ��0�c��þ�1�pþ�2�}@Lþ�3�þ�4�þ�5¤�þ�6±!þ�7�� Wþ>ӡxӡ>ӠxӠ	� � �	� �  � �	� � ۡ���ۣ�� ������ Oۡ��>.��	yӣ��۪��+۬��� ����� O۪�@�H>.��:yӬ�:%�� ��R��� O۠��o>.��ayӢ��R>�O!���>�O!���o	� �2%� �Ɛ'�@'O��� �(.�/8*�:0&���'�'�'�'G�� ��(�/8�:0������7>���0�
���D��V  

Program to configure the S-100 I/O board.
MAIN MENU
0 = Initilize the Zilog 8530 SCC chip(Ports = A0H-A3H)
1 = Set SCC Baud Rate
2 = SCC Loop test.  SCC chip(Port = A1H,A3H)
3 = DLP-USB Controller/Port Loop test (Port = A4H, 8255 Status = AAH))
4 = Send test speech string to V_Stamp chip via SCC chip (Port = A0H,A2H)
ESC = Return to CP/M

Menu choice ---->$

$The SCC is now initilized to 8 Bits, 1 Stop bit, NP, RTS on, 19,200 Baud.

$The SCC must first be initilized to 8 Bits, 1 Stop bit, NP, RTS on, 19,200 Baud.

$Select a Baud rate for this SCC (Both A & B Channels)
0 = 600 baud
1 = 1200 baud
2 = 2400 baud
3 = 4800 baud
4 = 9600 baud
5 = 19200 baud

6 = 38400 baud

7 = 76800 baud

Baud Rate number selected --->$Baud rate RESET to 600 baud (Both A & B Channels)
$Baud rate RESET to 1200 baud (Both A & B Channels)
$Baud rate RESET to 2400 baud (Both A & B Channels)
$Baud rate RESET to 4800 baud (Both A & B Channels)
$Baud rate RESET to 9600 baud (Both A & B Channels)
$Baud rate RESET to 19200 baud (Both A & B Channels)
$Baud rate RESET to 38400 baud (Both A & B Channels)
$Baud rate RESET to 76800 baud (Both A & B Channels)
$
Skipped selecting a new BAUD rate for the board current SCC.
$
Invalid menu item
$                                                                 �                                                                                          
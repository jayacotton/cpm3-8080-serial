: =2����W�-!�"�*�|��W+"�*�#"�>ͬ:�ͬ/ͬ �QY*�~ͬ�&�o��O~�W$^�#�@:���czͬKyͬ���)2�"����:��>�Ԭ�� ���y�qOK����:��ڨ���qReceived �*�ͽ�) blocks�>xͳ���ʯ��!�4~�
�Ϳ10 ACK errors�~���>�>
��~�#���2����W�-!�"�*�|��W+"�*�#"�>ͬ:�ͬ/ͬ �QY*�~ͬ�&�o��O~�W$^�#�@:���czͬKyͬ���)2�"����:��>�Ԭ�� ���y�qOK����:��ڨ���qReceived �*�ͽ�) blocks�>xͳ���ʯ��!�4~�
�Ϳ10 ACK errors�~���>�>
��~�#  0@P`p��������2"RBrb��������$4dtDT��������6&vfVF��������HXhx(8��������ZJzj
:*���뛋��l|L\,<���ݭ���~n^N>.���Ͽ����������� 0 P@p`��������"2BRbr��������4$tdTD��������&6fvFV���陉��XHxh8(��������JZjz
*:���ͽ���|l\L<,���߯���n~N^.> !Bc����)Jk����1sR����9{Z����bC �Ǥ�jK(	�Ϭ�Sr0����[z8�����冧@a#�펯Hi
+�Է�qP3�ܿ�yX;����"`A����*hI����2Qp����:Yx����-No����%Fg����=^����5wV�˨�nO,�à�fG$����_~<��Wv4Lm/�銫De'�Ⴃ}\?�ػ�uT7�г�.lM����&dE����>]|����6Ut����2�>ͳ�>2�2�ͱW:�_ͱ/����2� �QY*�ͱw�&�o��O~�W$^�#�2:���Xͱ���Kͱ���:�="�T*�#"�*�#"���:���<�ͿLost blocks�!�45���³�qSender canceled�>���ͪ�2�� ͱ��ͿCan,,t sync�ͱ��>ͬ!�4~�
�Ϳ10 bad blocks�*�|�7�>�2��!  "�&	"�(�z��'>�2���T]�"�� ��:���ͪ���� ����:��! 	"���"��� �*�#"�:���e�2�*�|��>ͬ���qOKSent �õ>�O�->2��*��-� ��������������KS{�T]<	���Ѻ�S�0O>�E-!+!�~<�5��u�6<H��>�E��>	�E��Ϳ^C�\ ���� ����Z:�O�{ͱ�2��[�V�3�2��u with CRCs����u with checksums���u������ͿInit timeout�ͿDisk write fail�G��UART Tx fail�G��by receiver�:�G�>��>ͬ�qABORT: ����x��-2����Ó�%FILE CLOSE FAIL! May be corrupt��%Empty file erased��u�����+}�̙>�E��.>	�D>�����O����* o�+}�̙>�E�T��+}�̙� ��d����!  +|�ʞ� ��yy����>Ó>�Ey���!�5*�����â	�( ��No /I1� y��+}�̙ ��No /I2� ʾ�            �        	    �C    1
��		>2��02�!] >/��T��	> w#w+#�	r#�$	� 2��d���d�c�l�=	�Fw���<2�#�"��"����p�2��            !�:��_��_ N#F#^#V��	:��ʜ	!Z"�ı�#^#V�"�:�!  	=±	"�:����	�qSend or receive (S/R)? ���
�R���	/2��qFile �:���
!3"���$�uopenSend�ͭ
�
��R
�uexists. Overwrite (Y/N)?���
�Y�-��qFile ��ucreate���1�udReceiv�ͭ
!|
"���!�"�:���I�]:��§
:�G!���+}�

:���uing via �:�=��
=��
��
�ucustom code���udirect I/O���uRDR/PUN��2��uCON��� >
O�"=�����d���;� �/���G��2�2�!?�#N#���1�G	�CEIK�M9OEPRQqRtSsXuZ��2��<2���G�0���!�O	N	͘�{�w#�t��u��G���Æ��2�͘�� ÖlaO��¡G����!�ͅx!kͅ��=2��<2���G�0���2���G�1���<2���G�G����O�l����Gx=�@��x���	G: ��x=2���0�
����G� �4:��7��#��"��"���]6�!�5�'����
��7?�>�2����y��d��y��/7?�4���G�� ��	7?�s##w#y�ʑ6�#####r���dͺҵ�����G�Gͺ������4�7��0�
�������͘��q/& bad value����q/& unknown����qJunk��u in �:����)XMODEM.CFG��)command line��)not found��) fail. Write protect? Dir full?��%No filename��q========================Xmodem 2.9 By M.Eberhard========================Usage: XMODEM <file> <option list>^C aborts
Command line and XMODEM.CFG options: /R Receive, /S Send /C Receive with checksums, else CRCs   (Receiver always sets error check mode) /E if CP/M RDR returns with Z set when not ready /Knn sets buffer max k-bytes (default: all free RAM)   nn is decimal, 0<nn<64
--More--���q /In 8080 code patches for /X3 I/O routines:  /I0 h0 h1 ...h11: initialize  /I1 h0 h1 ...h11: Tx data, chr in c  /I2 h0 h1 ...h11: Rx status, Z set if no chr  /I3 h0 h1 ...h11: Rx data, chr in a /M Console message /O pp h0 h1...hn sends hex h1-hn to port pp /P ss dd qq rr tt defines direct I/O port:  ss: status port  dd: data port  qq: 00/01 for active low/high ready  rr: Rx ready bit mask  tt: Tx ready bit mask/I, /O and /P values are 2-digit hex
--More--���% /Q for Quiet: else + means ok block, - means retry /X sets the transfer port:  /X0 CP/M CON *  /X1 CP/M RDR/PUN * (default)  /X2 Direct I/O, defined by /P option  /X3 Custom code from /I patches  * Must not strip parity /Zm for m MHz CPU. 0<m<7, default m=2��4�;B��$ju��� XMODEM  CFG                                                                                                                     
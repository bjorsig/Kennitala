from unittest import TestCase
from kennitala import Kennitala
import time

__author__ = 'Björgvin'


class TestKennitala(TestCase):
    def test_er_logleg_logleg(self):
        self.assertTrue(Kennitala("1111111119").er_logleg())
        self.assertTrue(Kennitala("6503760649").er_logleg()) #fyrirtækjakennitala

    def test_er_logleg_ologleg(self):
        self.assertFalse(Kennitala("010101").er_logleg()) #of stutt
        self.assertFalse(Kennitala("abcdefghij").er_logleg()) #ekki tölustafir
        self.assertFalse(Kennitala("1709715049").er_logleg()) #stenst ekki vartölupróf

    def test_fa_faedingardag(self):
        kt0 = Kennitala('0209724249')
        kt1 = Kennitala('1709715079')
        kt2 = Kennitala('2111972859')

        str_kt3 = '30119344'
        vartala = Kennitala(str_kt3).fa_vartolu()
        kt3 = Kennitala(str_kt3 + str(vartala) + '8')

        kt4 = Kennitala('4807042310')
        kt5 = Kennitala('7103042110')

        format = '%Y%m%d' #YYYYmmdd
        dt0 = time.strptime('19720902', format)
        dt1 = time.strptime('19710917', format)
        dt2 = time.strptime('19971121', format)
        dt3 = time.strptime('18931130', format)
        dt4 = time.strptime('20040708', format)
        dt5 = time.strptime('20040331', format)

        #self.assertEqual(dt0, kt0.fa_faedingardag(), 'kt0 ekki rétt')
        self.assertEqual(dt1, kt1.fa_faedingardag(), 'kt1 ekki rétt')
        self.assertEqual(dt2, kt2.fa_faedingardag(), 'kt2 ekki rétt')
        self.assertEqual(dt3, kt3.fa_faedingardag(), 'kt3 ekki rétt')
        self.assertEqual(dt4, kt4.fa_faedingardag(), 'kt4 ekki rétt')
        self.assertEqual(dt5, kt5.fa_faedingardag(), 'kt5 ekki rétt')


    def test_fa_vartolu(self):
        kt0 = Kennitala('1709715079')
        kt1 = Kennitala('5901690809')

        self.assertEqual(kt0.fa_vartolu(), 7, 'kt0 ekki rétt vartala')
        self.assertEqual(kt1.fa_vartolu(), 0, 'kt1 ekki rétt vartala')

    def test_tegund_kennitolu(self):
        kt_eins = Kennitala('2222222229')
        kt_fyr = Kennitala('6809780429')
        self.assertEqual('EINST', kt_eins.tegund_kennitolu())
        self.assertEqual('FYRIRT', kt_fyr.tegund_kennitolu())
        #Sself.assertRaises()

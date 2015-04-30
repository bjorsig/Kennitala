 # -*- coding: utf-8 -*-

__author__ = 'Björgvin'

#########################################
#
#
#########################################

import time

class Kennitala:
    'Klasi sem þekkir íslenskar kennitölur, getur sagt til um lögmæti og vartölupróf og skilað fæðingardegi'

    margfeldi = [3,2,7,6,5,4,3,2]

    def __init__(self):
        'Default initialization'
        self.kt = ''


    def __init__(self, kennitala):
        'Initializaiton sem tekur inn kennitölustreng '
        self.kt = kennitala


    def er_logleg(self):
        'Segir til um hvort kenitala er lögleg, þ.e. "well formed". En ekki hvort hún tilheyri einhverjum.'
        if len(self.kt) != 10:
            return False
        elif self.kt[9] not in ['8', '9', '0']:
            return False
        else:
            try:
                if self.fa_vartolu(self.kt) == int(self.kt[8]):
                    return True
                else:
                    return False
            except:
                return False


    def fa_faedingardag(self):
        'Skilar fæðingardegi eða stofndegi fyritækis úr viðkomandi kennitölu'
        if self.er_logleg():
            ar = 0
            if self.kt[9] == '8':
                ar = 1800 + int(self.kt[4:6])
            elif self.kt[9] == '9':
                ar = 1900 + int(self.kt[4:6])
            elif self.kt[9] == '0':
                ar = 2000 + int(self.kt[4:6])

            if self.tegund_kennitolu() == 'FYRIRT':
                day = int(self.kt[0]) - 4
                day = str(day) + self.kt[1]
                return time.strptime(str(ar) + self.kt[2:4] + str(day), '%Y%m%d')
            elif self.tegund_kennitolu() == 'EINST':
                return time.strptime((str(ar) + self.kt[2:4] + self.kt[0:2]), '%Y%m%d')
        raise "Ekki hægt að mynda dagsetningu, kennitala ekki lögleg"(self.kt)


    def fa_vartolu(self, kennitoluhluti = ''):
        'Skilar vartölu fyrir kennitöluhluta þar sem kennitöluhluti er fyrstu 8 stafir í kennitölu. Ef engin kennitöluhluti er skilgreindur er kennitala klasa notuð.'

        if len(kennitoluhluti) == 0 and len(self.kt) > 0:
            kennitoluhluti = self.kt

        if len(kennitoluhluti) < 8:
            raise "Kennitöluhluti þarf að vera minnst 8 stafir"(kennitoluhluti)
        else:
            try:
                sum = 0
                for i in range(len(Kennitala.margfeldi)):
                    sum = sum + Kennitala.margfeldi[i] * int(kennitoluhluti[i])
                leif = sum % 11
                vartala = 11 - leif
                if vartala == 11:
                    return 0
                elif vartala == 10:
                    raise "Kennitöluhluti getur ekki verið hluti af löglegri kennitölu"(kennitoluhluti)
                else:
                    return 11 - leif
            except TypeError:
                raise "Kennitöluhluti þarf að vera 8 tölustafa strengur"(kennitoluhluti)


    def tegund_kennitolu(self):
        if self.er_logleg():
            if self.kt[0] in set(['0', '1', '2', '3']):
                return 'EINST'
            elif self.kt[0] in set(['4', '5', '6', '7']):
                return "FYRIRT"
        raise "Kennitala ekki lögleg"(self.kt)


def er_logleg(kennitala):
    'Skilar boolean gildi sem segir til um hvort kennitala sé lögleg, þ.e. rétt formuð með réttri vartölu.'
    return Kennitala(kennitala).er_logleg()

def fa_vartolu(kennitöluhluti):
    'Reiknar og skilar vartölu (9. staf) í kennitölu fyrir skilgreindan kennitöluhluta sem verður að vera amk 8 tölustafir.'
    return Kennitala().fa_vartolu(kennitöluhluti)

def fa_faedingardag(kennitala):
    'Skilar fæðingardegi sem time_struct.'
    return Kennitala(kennitala).fa_faedingardag()

def tegund_kennitolu(kennitala):
    return Kennitala(kennitala).tegund_kennitolu()
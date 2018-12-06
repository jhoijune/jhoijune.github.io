def interpreter(data,var_dict=None):
    """ 어셈블리어를 10진수의 수로 변환

    """
    INSTRUCTION = {"AND":0X0000,"ADD":0X1000,"LDA":0X2000,"STA":0X3000,"BUN":0X4000,"BSA":0X5000,"ISZ":0x6000}
    REGISTER = {"CLA":0X7800,"CLE":0X7400,"CMA":0X7200,"CME":0X7100,"CIR":0X7080,"CIL":0X7040,
    "INC":0X7020,"SPA":0X7010,"SNA":0X7008,"SZA":0X7004,"SZE":0X7002,"HLT":0X7001}
    IO = {"INP":0XF800,"OUT":0XF400,"SKI":0XF200,"SKO":0XF100,"ION":0XF080,"IOF":0XF040}
    if data[:3] in INSTRUCTION:
        head = INSTRUCTION[data[:3]]
        if data[-1] == "I":
            head += 0x8000
            temp = data[4:]
            loc = temp[:len(temp)-2]
        else:
            loc = data[4:]
        if var_dict != None and loc in var_dict:
            return head + var_dict[loc]
        else:
            try:
                return head + int(loc,16)
            except:
                raise FormError("주소부분인 " + loc + "가 16진수가 아닙니다.")        
    elif data in REGISTER:
        return REGISTER[data]
    elif data in IO:
        return IO[data]
    else:
        raise FormError("명령어 양식이 잘못되었습니다.")




class FormError(Exception):
    pass

class Halt(Exception):
    pass

def convert_hex(num):
    """
    숫자를 4자리 16진수로 변환함
    """
    if num >= 0 :
        return format(num,"X").zfill(4)
    else:
        return format(0XFFFF + num + 1,"X").zfill(4)

def correct_bin(bin_num,digit=16):
    return bin_num.zfill(digit)

class CPU:

    INDIRECT_DET = 8 # indirect 인지 판단
    REGISTER_DET = 7 # 레지스터를 참조하는지 판단
    INSTRUCTION_LIST = ["AND","ADD","LDA","STA","BUN","BSA","ISZ","AND","ADD",
    "LDA","STA","BUN","BSA","ISZ","CLA","CLE","CMA","CME","CIR","CIL",
    "INC","SPA","SNA","SZA","SZE","HLT","INP","OUT","SKI","SKO","ION","IOF"]

    def __init__(self,byte_len=2**10):
        self._PC = None # 프로그램 카운터: ORG를 통해 초기화 해야함
        self._AC = 0 # 데이터를 일시적으로 보관하는 누산기 
        self._AR = 0 # 주소 레지스터 
        self._IR = 0 # 실행될 명령어의 연산코드를 저장  
        self._TR = 0 # 데이터를 일시적으로 저장
        self._DR = 0 # 메모리 연산자 저장    
        self._INPR = 0 # 인풋 데이터를 hold
        self._OUTR = 0 # 아웃풋 데이터를 hold 
        self._E = 0
        self._M = [0] * byte_len # 메모리
        self._S = 1 # S = 0이면 컴퓨터를 멈춤
        self._cmd_lst = list() # 커맨드 리스트 
        self._inputable = True # 커맨드 리스트 입력가능인지 
        self._det = False # 컴파일 판단
        self._INSTRUCTION = {"0":"AND","1":"ADD","2":"LDA","3":"STA","4":"BUN","5":"BSA","6":"ISZ",
        "8":"AND","9":"ADD","A":"LDA","B":"STA","C":"BUN","D":"BSA","E":"ISZ",
        "7800":"CLA","7400":"CLE","7200":"CMA","7100":"CME","7080":"CIR","7040":"CIL",
        "7020":"INC","7010":"SPA","7008":"SNA","7004":"SZA","7002":"SZE","7001":"HLT",
        "F800":"INP","F400":"OUT","F200":"SKI","F100":"SKO","F080":"ION","F040":"IOF"}
    
    def input_command(self):
        """커맨드 여러 줄 입력
        
        """
        print("명령어를 입력하세요: (엔터로 구분)")
        while self._inputable:
            cmd = input()
            if cmd == "END":
                self._cmd_lst.append(cmd)
                self._inputable = False
            elif cmd == "":
                break
            else:
                self._cmd_lst.append(cmd)

    def allocate_command(self,line_num=None):
        """커맨드 한 줄 입력
        
        """
        if self._inputable:
            if line_num == None:
                print("명령어를 입력하세요: ")
                cmd = input()
                if cmd == "END":
                    self._cmd_lst.append(cmd)
                    self._inputable = False
                elif cmd == "":
                    pass
                else:
                    self._cmd_lst.append(cmd)
            elif line_num > 0:
                print("명령어를 입력하세요: ")
                cmd = input()
                if cmd == "END":
                    try:
                        self._cmd_lst.insert(line_num-1,cmd)
                        self._inputable = False    
                    except:
                        print("라인 {0}에 명령어를 할당할 수 없습니다.".format(line_num))  
                elif cmd == "":
                    pass
                else:
                    try:
                        self._cmd_lst.insert(line_num-1,cmd)    
                    except:
                        print("라인 {0}에 명령어를 할당할 수 없습니다.".format(line_num))    
            else:
                print("라인 번호는 0이나 음수가 될 수 없습니다.") 
        else:
            print("END가 있어 명령어를 할당할 수 없습니다.")


                    
    def clear_memory(self):
        """메모리 0으로 초기화 함
        
        """
        self._M = [0] * len(self._M)
        self._INSTRUCTION = {"0":"AND","1":"ADD","2":"LDA","3":"STA","4":"BUN","5":"BSA","6":"ISZ",
        "8":"AND","9":"ADD","A":"LDA","B":"STA","C":"BUN","D":"BSA","E":"ISZ",
        "7800":"CLA","7400":"CLE","7200":"CMA","7100":"CME","7080":"CIR","7040":"CIL",
        "7020":"INC","7010":"SPA","7008":"SNA","7004":"SZA","7002":"SZE","7001":"HLT",
        "F800":"INP","F400":"OUT","F200":"SKI","F100":"SKO","F080":"ION","F040":"IOF"}
        self._det = False
    
    def clear_instruction(self):
        """명령어 리스트 초기화 함
        
        """
        self._cmd_lst = list()
        self._inputable = True
        self.clear_memory()
    
    def check_instruction(self):
        """명령어 리스트 내부 출력
        
        """
        if len(self._cmd_lst) != 0:
            for line_num in range(len(self._cmd_lst)):
                print("Line {0} | {1}".format(str(line_num + 1).zfill(3),self._cmd_lst[line_num]))
        else:
            print("명령어 리스트가 없습니다.")

    def check_symbol(self,hex_num): 
        """문자인 16진수를 판단해 symbol로 변환 함
           
           hex_num 16진수 string
        
        """        
        hex_four = int(hex_num[0],16)
        if hex_four % CPU.INDIRECT_DET != CPU.REGISTER_DET:
            d_or_id = "" if hex_four // CPU.INDIRECT_DET == 0 else " I"
            if hex_num[1:] in self._INSTRUCTION:
                symbol = self._INSTRUCTION[hex_num[0]] + " " + self._INSTRUCTION[hex_num[1:]] + d_or_id            
            else:
                symbol = self._INSTRUCTION[hex_num[0]] + " " + hex_num[1:] + d_or_id
        else:
            if hex_num in self._INSTRUCTION:
                symbol = self._INSTRUCTION[hex_num]
            else:
                symbol = "nop"
        return symbol

    def compile(self):
        """명령어 컴파일 해 메모리에 숫자로 바꿈
        
        """
        var_names = dict()
        # 전체 명령어 판단
        if self._cmd_lst[0].find("ORG") != -1:
            self._ORG = int(self._cmd_lst[0].split()[1],16)
            mc = self._ORG
        else:
            raise FormError("명령어의 처음이 ORG문이 아닙니다.")
        for ind in range(1,len(self._cmd_lst)):
            if self._cmd_lst[ind][:3] == "ORG":
                mc = int(self._cmd_lst[ind][4:],16)
            elif self._cmd_lst[ind].find(",") != -1: 
                temp = self._cmd_lst[ind]
                var_name,mb_part  = temp.split(",")
                var_name,mb_part = var_name.strip(),mb_part.strip()
                if mb_part.find(" ") != -1:
                    part_lst = mb_part.strip().split()
                    if len(part_lst) == 3:
                        middle_part,back_part,I_part = part_lst
                        middle_part = middle_part.strip()
                        back_part = back_part.strip()
                        I_part = I_part.strip()
                    elif len(part_lst) == 2:
                        middle_part,back_part = part_lst
                        middle_part = middle_part.strip()
                        back_part = back_part.strip()
                    else:
                        raise FormError("형식이 잘못되었습니다.")
                else:
                    pass
                if mb_part in CPU.INSTRUCTION_LIST or middle_part in CPU.INSTRUCTION_LIST:
                    var_names[var_name] = mc
                    self._M[mc] = mb_part
                    self._INSTRUCTION[convert_hex(mc)[1:]] = var_name
                    mc += 1
                else:
                    if len(var_name) == 3:
                        try:
                            int(var_name,16) # 숫자로 바뀌면 충돌이 일어남
                            self.clear_memory()
                            raise NameError("변수명 {0}은 옳지 않습니다.".format(var_name))
                        except:
                            pass
                    else:
                        pass 
                    var_names[var_name] = mc 
                    self._INSTRUCTION[convert_hex(mc)[1:]] = var_name
                    if middle_part == "DEC":
                        self._M[mc] = int(back_part)
                        mc += 1 
                    elif middle_part == "HEX":
                        self._M[mc] = int(back_part,16)
                        mc += 1
                    else:
                        self.clear_memory()
                        raise FormError("변수 진법 선언 형식이 맞지 않습니다.")
            elif len(self._cmd_lst[ind]) == 4:
                # 익명 변수 숫자 선언 (16진수 가정)
                try:
                    self._M[mc] = int(self._cmd_lst[ind],16)
                    mc += 1
                except:
                    self.clear_memory()
                    raise FormError("형식이 맞지 않습니다. 16진수여야 합니다.")
            elif self._cmd_lst[ind][:3] == "DEC":
                # 익명 변수 형식 있을 때 (10진수)
                self._M[mc] = int(self._cmd_lst[ind][4:])
                mc += 1
            elif self._cmd_lst[ind][:3] == "HEX":
                # 익명 변수 형식 있을 때 (16진수)
                self._M[mc] = int(self._cmd_lst[ind][4:],16)
                mc += 1
            elif self._cmd_lst[ind][:3] == "END":
                break
            else:
                # 일반 명령문일 때
                self._M[mc] = self._cmd_lst[ind]
                mc += 1
        # 명령어 10진수 번역
        for ind in range(len(self._M)):
            if isinstance(self._M[ind],str):
                try:
                    self._M[ind] = interpreter(self._M[ind],var_names)
                except:
                    self.clear_memory()
                    raise FormError("형식이 맞지 않습니다.")
        self._det = True
    
    def _start(self):
        """ 시작 메소드

        """
        self._SC = 0
    
    def _action_of_T0(self):
        self._SC += 1
        self._AR = self._PC
        print("----------------T0----------------")
        print("AR <- PC")
        print("AR = {0}".format(convert_hex(self._AR))) 

    def _action_of_T1(self):
        self._SC += 1
        self._IR = self._M[self._AR]
        self._PC += 1
        print("----------------T1----------------")
        print("IR <- M[AR], PC <- PC + 1")
        print("IR = {0}, PC = {1}".format(convert_hex(self._M[self._AR]),convert_hex(self._PC))) 

    def _action_of_T2(self):
        self._SC += 1
        self.hex_4 = int(convert_hex(self._IR)[0],16) # 16진수 4번째 수
        self._D7 = 1 if self.hex_4 % CPU.INDIRECT_DET == CPU.REGISTER_DET else 0
        self._AR = int(convert_hex(self._IR)[1:],16) 
        self._I = self.hex_4 // CPU.INDIRECT_DET    
        print("----------------T2----------------")
        print("Decode operation code in IR(12-14)")
        print("AR <- IR(0-11), I <- IR(15)")
        print("AR = {0}, I = {1}".format(convert_hex(self._AR)[1:],self._I))
        print("D7 = {0}".format(self._D7))

    def _decide_instruction(self):
        hex_ir = convert_hex(self._IR) # 16진수 수
        symbol = self.check_symbol(hex_ir) 
        print("instruction : {0}".format(symbol))
        if self._D7 == 1:
            if self._I == 0:
                print("----------------T3----------------")
                self._SC += 1
                print("Excute register-reference instruction")
                if symbol == "CLA": self._CLA()
                elif symbol == "CLE": self._CLE()
                elif symbol == "CMA": self._CMA()
                elif symbol == "CME": self._CME()
                elif symbol == "CIR": self._CIR()
                elif symbol == "CIL": self._CIL()
                elif symbol == "INC": self._INC()
                elif symbol == "SPA": self._SPA()
                elif symbol == "SNA": self._SNA()
                elif symbol == "SZA": self._SZA()
                elif symbol == "SZE": self._SZE()
                elif symbol == "HLT": self._HLT()
                else:raise FormError("양식이 잘못되었습니다.")
                print("SC <- 0")
                self._SC = 0
            else:
                print("----------------T3----------------")
                self._SC += 1
                print("Excute input-output instruction")
                if symbol == "INP": self._INP()
                elif symbol == "OUT": self._OUT()
                elif symbol == "SKI": self._SKI()
                elif symbol == "SKO": self._SKO()
                elif symbol == "ION": self._ION()
                elif symbol == "IOF": self._IOF()
                else:raise FormError("양식이 잘못되었습니다.")
                print("SC <- 0")
                self._SC = 0
        else:
            if self._I == 1:
                print("----------------T3----------------")
                self._SC += 1
                self._AR = self._M[self._AR]
                print("AR <- M[AR]")
                print("AR = {0}".format(convert_hex(self._AR)))
            else:
                print("----------------T3----------------")
                self._SC += 1
            if symbol[:3] == "AND": self._AND()
            elif symbol[:3] == "ADD": self._ADD()
            elif symbol[:3] == "LDA": self._LDA()
            elif symbol[:3] == "STA": self._STA()
            elif symbol[:3] == "BUN": self._BUN()
            elif symbol[:3] == "BSA": self._BSA()
            elif symbol[:3] == "ISZ": self._ISZ()
            else:raise FormError("양식이 잘못되었습니다.")
            print("SC <- 0")
            self._SC = 0

    def execute(self):
        """ 컴파일 됐으면 명령어를 수행
        
        """
        if self._det:
            try:
                self._PC = self._ORG
                self._start()
                while True:
                    self._action_of_T0()
                    self._action_of_T1()
                    self._action_of_T2()
                    self._decide_instruction()        
            except Halt as e:
                print(e)
            except:
                print("명령이 잘못되었습니다.")
        else:
            raise Halt("컴파일 하지 않았습니다.")

    def print_inside(self):
        """ 메모리와 레지스터 상태를 출력
        
        """
        print("메모리와 레지스터의 현재 상태")
        for ind in range(len(self._M)):
            if self._M[ind] != 0:
                print("M[{0}] = {1:>5}".format(convert_hex(ind),convert_hex(self._M[ind])))
            else:
                pass 
        print("----------------------------------------")
        print("PC: {0:>5}".format(convert_hex(self._PC)))
        print("AR: {0:>5}".format(convert_hex(self._AR)))
        print("AC: {0:>5}".format(convert_hex(self._AC)))
        print("IR: {0:>5}".format(convert_hex(self._IR)))
        print("TR: {0:>5}".format(convert_hex(self._TR)))
        print("DR: {0:>5}".format(convert_hex(self._DR)))

    # 레지스터 명령
    
    def _CLA(self):
        self._AC = 0
        print("AC <- 0")
        print("AC = {0}".format(convert_hex(self._AC)))   
    def _CLE(self):
        self._E = 0
        print("E <- 0")
        print("E = {0}".format(self._E))   
    def _CMA(self):
        self._AC = ~self._AC 
        print("AC <- ~ AC")
        print("AC = {0}".format(convert_hex(self._AC)))   
    def _CME(self):
        self._E = 1 if self._E == 0 else 0
        print("E <- ~ E")
        print("E = {0}".format(self._E))   
    def _CIR(self):
        AC_bin = bin(self._AC)[2:]
        AC_bin = correct_bin(AC_bin)
        self._AC = int(str(self._E)+AC_bin[0:15],2)
        self._E = int(AC_bin[-1],2)
        print("AC <- shr AC, AC(15) <- E, E <- AC(0)")
        print("AC : {0}, E: {1}".format(convert_hex(self._AC),self._E))   
    def _CIL(self):
        AC_bin = bin(self._AC)[2:]
        AC_bin = correct_bin(AC_bin)
        self._AC = int(AC_bin[1:] + str(self._E),2)
        self._E = int(AC_bin[0],2)
        print("AC <- shl AC, AC(0) <- E, E <- AC(15)")
        print("AC : {0}, E: {1}".format(convert_hex(self._AC),self._E))    
    def _INC(self):
        self._AC += 1
        print("AC <- AC + 1")
        print("AC : {0}".format(convert_hex(self._AC)))   
    def _SPA(self):
        AC_I = int(convert_hex(self._AC)[0],16) // CPU.INDIRECT_DET
        if AC_I == 0:
            self._PC += 1
        print("If(AC(15) = 0) then (PC <- PC + 1)")
        print("PC : {0}".format(convert_hex(self._PC)))
        
    def _SNA(self):
        AC_I = int(convert_hex(self._AC)[0],16) // CPU.INDIRECT_DET
        if AC_I == 1:
            self._PC += 1
        print("If(AC(15) = 1) then (PC <- PC + 1)")
        print("PC : {0}".format(convert_hex(self._PC)))
        
    def _SZA(self):
        if self._AC == 0:
            self._PC += 1
        print("If(AC = 0) then (PC <- PC + 1)")
        print("AC: {0}, PC : {1}".format(convert_hex(self._AC),convert_hex(self._PC)))       
    
    def _SZE(self):
        if self._E == 0:
            self._PC += 1
        print("If(E = 0) then (PC <- PC + 1)")
        print("E : {0}, PC : {1}".format(self._E,convert_hex(self._PC)))       
    
    def _HLT(self):
        self._S = 0
        if self._S == 0:
            print("----------------------------------")
            raise Halt("컴퓨터를 정지합니다.")
    
    # IO 명령

    def _INP(self):
        self._AC = self._INPR
        self._FGI = 0
        print("AC(0-7) <- INPR, FGI <- 0")
        print("AC(0-7) = {0} , FGI = {1}".format(convert_hex(self._AC),self._FGI)) 
    def _OUT(self):
        self._OUTR = self._AC
        self._FGO = 0
        print("OUTR <- AC(0-7), FGO <- 0")
        print("OUTR = {0}, FGO = {1}".format(self._OUTR,self._FGO))
    def _SKI(self):
        if self._FGI == 1:
            self._PC += 1
        print("If(FGI = 1) then (PC <- PC + 1)")
        print("FGI = {0}, PC = {1}".format(self._FGI,convert_hex(self._PC)))   
    def _SKO(self):
        if self._FGO == 1:
            self._PC += 1
        print("If(FGO = 1) then (PC <- PC + 1)")
        print("FGO = {0}, PC = {1}".format(self._FGO,convert_hex(self._PC)))   
    def _ION(self):
        self._IEN = 1 
        print("IEN <- 1")
    def _IOF(self):
        self._IEN = 0
        print("IEN <- 0")   

    # 통상적인 명령
    
    def _AND(self):
        print("----------------T4----------------")
        self._SC += 1
        self._DR = self._M[self._AR]
        print("DR <- M[AR]")
        print("DR = {0}".format(convert_hex(self._DR)))
        print("----------------T5----------------")
        self._SC += 1
        self._AC = self._AC & self._DR 
        print("AC <- AC ^ DR")
        print("AC = {0}".format(convert_hex(self._AC)))     
    
    def _ADD(self):
        print("----------------T4----------------")
        self._SC += 1
        self._DR = self._M[self._AR]
        print("DR <- M[AR]")
        print("DR = {0}".format(convert_hex(self._DR)))
        print("----------------T5----------------")
        self._SC += 1
        ac_hex,dr_hex = int(convert_hex(self._AC),16),int(convert_hex(self._AC),16)
        if ac_hex + dr_hex > 0XFFFF: 
            temp_sum = (ac_hex + dr_hex - 0x10000) # 오버 플로우 재현
            self._AC = temp_sum if temp_sum < 0X8000 else -(temp_sum + 1)
            Cout = 1
        else:
            self._AC = self._AC + self._DR
            Cout = 0
        self._E = Cout
        print("AC <- AC + DR")
        print("E <- Cout")
        print("AC = {0}, E = {1}".format(convert_hex(self._AC),self._E))
    
    def _LDA(self):
        print("----------------T4----------------")
        self._SC += 1
        self._DR = self._M[self._AR]
        print("DR <- M[AR]")
        print("DR = {0}".format(convert_hex(self._DR)))
        print("----------------T5----------------")
        self._SC += 1
        self._AC = self._DR
        print("AC <- DR")
        print("AC = {0}".format(convert_hex(self._AC)))
    
    def _STA(self):
        print("----------------T4----------------")
        self._SC += 1
        self._M[self._AR] = self._AC
        print("M[AR] <- AC")
        print("M[AR] = {0}".format(convert_hex(self._M[self._AR])))
    
    def _BUN(self):
        print("----------------T4----------------")
        self._SC += 1
        self._PC = self._AR
        print("PC <- AR")
        print("PC = {0}".format(convert_hex(self._PC)))
    
    def _BSA(self):
        print("----------------T4----------------")
        self._SC += 1
        self._M[self._AR] = self._PC
        self._AR += 1
        print("M[AR] <- PC")
        print("AR <- AR + 1")
        print("M[AR] = {0}, AR = {1}".format(convert_hex(self._M[self._AR-1]),convert_hex(self._AR)))
        print("----------------T5----------------")
        self._SC += 1
        self._PC = self._AR
        print("PC <- AR")
        print("PC = {0}".format(convert_hex(self._PC)))     
    
    def _ISZ(self):
        print("----------------T4----------------")
        self._SC += 1
        self._DR = self._M[self._AR]
        print("DR <- M[AR]")
        print("DR = {0}".format(convert_hex(self._DR)))
        print("----------------T5----------------")
        self._SC += 1
        self._DR += 1
        print("DR = DR + 1")
        print("DR = {0}".format(convert_hex(self._DR)))
        print("----------------T6----------------")
        self._SC += 1
        self._M[self._AR] = self._DR
        if self._DR == 0:
        	self._PC += 1
        print("M[AR] <- DR")
        print("If(DR = 0) then (PC <- PC + 1)")
        print("M[AR] = {0}, PC = {1}".format(convert_hex(self._M[self._AR]),convert_hex(self._PC)))
    
        
def main1():
    # 표 6-5
    cpu = CPU()
    cpu._cmd_lst = ["ORG 0","LDA A","ADD B","STA C","HLT","A, DEC 83","B, DEC -23","C, DEC 0","END"]
    cpu.check_instruction()
    cpu.compile()
    cpu.execute()
    cpu.print_inside()

def main2():
    # 표 6-8
    cpu = CPU()
    cpu._cmd_lst = ["ORG 100","LDA SUB","CMA","INC","ADD MIN","STA DIF","HLT","MIN, DEC 83",
    "SUB, DEC -23","DIF, HEX 0","END"]
    cpu.check_instruction()
    cpu.compile()
    cpu.execute()
    cpu.print_inside()

def main3():
    # 표 6-14
    cpu = CPU()
    cpu._cmd_lst = ["ORG 100","LOP, CLE","LDA Y","CIR","STA Y","SZE","BUN ONE","BUN ZRO","ONE, LDA X",
    "ADD P","STA P","CLE","ZRO, LDA X","CIL","STA X","ISZ CTR","BUN LOP","HLT","CTR, DEC -8","X, HEX 000F",
    "Y, HEX 000B","P, HEX 0","END"]
    cpu.check_instruction()
    cpu.compile()
    cpu.execute()
    cpu.print_inside()

def main4():
    # 표 6-16
    cpu = CPU()
    cpu._cmd_lst = ["ORG 100","LDA X","BSA SH4","STA X","LDA Y","BSA SH4","STA Y","HLT",
    "X, HEX 1234","Y, HEX 4321","SH4, HEX 0","CIL","CIL","CIL","CIL","AND MSK","BUN SH4 I",
    "MSK, HEX FFF0","END"]
    cpu.check_instruction()
    cpu.compile()
    cpu.execute()
    cpu.print_inside()

def main5():
    # 표 6-17
    cpu = CPU()
    cpu._cmd_lst = ["ORG 200","LDA X","BSA OR","HEX 3AF6","STA Y","HLT","X, HEX 7B95","Y, HEX 0",
    "OR, HEX 0","CMA","STA TMP","LDA OR I","CMA","AND TMP","CMA","ISZ OR","BUN OR I","TMP, HEX 0","END"]
    cpu.check_instruction()
    cpu.compile()
    cpu.execute()
    cpu.print_inside()



def main():
    cpu = CPU()
    cpu.input_command()

main1()
main2()
main3()
main4()
main5()
    


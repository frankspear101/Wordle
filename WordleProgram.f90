	Program WordleSolve
!	Program designed to parse the Wordle word list based on the known and excluded letters
!	Parsing is done by
!		Removing all words with excluded letters
!		Removing all words that don't have a yellow letter
!		Removing all words that don't have a green letter in the right spot

	integer*4 i,j,k,numWords,iopt,where
	character*5 Words(5000)
	character*1 aLetter



!	Read the Wordle word list
	write(*,*)'**************************'
	write(*,*)'Open Wordle Word file'
	write(*,*)'**************************'
	
	open(20,file='')
!	open(20,file='Wordle word list answers.txt')
	i = 0
	do
		i = i + 1
		read(20,*,end=10)Words(i)
		end do
10	continue
	numWords = i - 1
1	continue
	write(*,*)'-------------------'
	write(*,*)' Words left = ',numWords
	write(*,*)'Options'
	write(*,*)' 1 = list remaining words'
	write(*,*)' 2 = include a letter in a fixed spot'
	write(*,*)' 3 = include a letter and exclude a spot'
	write(*,*)' 4 = exclude a letter'
	write(*,*)' 5 = restart'
	read(*,*,err=99)iopt
	
	select case(iopt)
	
	case(1)
		do i = 1,numWords
			write(*,*)Words(i)
			end do
	case(2)
		write(*,*)' Input the letter to include'
		read(*,*,err=99)aLetter
		write(*,*)' Where does this letter go?'
		write(*,*)' 1-5 to fix the spot'
		read(*,*,err=99)where
		k = 0
		do i = 1,numWords
			j = where
			if(aLetter.eq.Words(i)(j:j))then
				! we have matched a letter in the right spot - this word is OK
				k = k + 1
				Words(k) = Words(i)
				endif
			end do	! loop on all words left
		numWords = k

	case(3)

		write(*,*)' Input the letter to include'
		read(*,*,err=99)aLetter
		write(*,*)' Where does this letter NOT go?'
		write(*,*)' 1-5 to exclude the spot'
		read(*,*,err=99)where

		! First make sure the letter is not present in the exclude spot
		k = 0
		do i = 1,numWords
			j = where
			if(aLetter.eq.Words(i)(j:j))cycle
			! The letter is not present in the exclude spot
			k = k + 1
			Words(k) = Words(i)
			end do	! loop on all words left
		numWords = k

		! Now make sure the letter is in the word somewhere other than the exclude spot
		k = 0
		do i = 1,numWords
			do j = 1,5
				!if(aLetter.eq.Words(i)(j:j))then
				if(aLetter.eq.Words(i)(j:j).and.j.ne.where)then
					! we have matched a letter - this word is OK
					k = k + 1
					Words(k) = Words(i)
					go to 20
					endif
				end do
			20 continue
			end do	! loop on all words left
		numWords = k

		
	
	case(4)
		write(*,*)' Input the letter to exclude'
		read(*,*,err=99)aLetter
		k = 0
		do i = 1,numWords
			do j = 1,5
				if(aLetter.eq.Words(i)(j:j))go to 30
				end do
			! if here, then this word is OK
			k = k + 1
			Words(k) = Words(i)
30			continue
			! if we skipped to here then this word gets excluded
			! get the next word
			end do			
		numWords = k

	case (5)
		rewind(20)
		i = 0
		do
			i = i + 1
			read(20,*,end=11)Words(i)
			end do
11		continue
		numWords = i - 1
		go to 1
	case default
		go to 1
	end select
	go to 1
99	continue
	write(*,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
	write(*,*)'Input error -- try again'
	go to 1
	
	end
	
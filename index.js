let remove = document.querySelectorAll('.remove');
let replace = document.querySelectorAll('.replace');
let imageCont = document.querySelectorAll('.images');

remove.forEach((item, index) => {
item.addEventListener('click', () => {
    let tl = gsap.timeline();
    tl.to(item, { opacity: 0, duration: 0.5 })
    .set(item, { display: 'none' })
    .set(replace[index], { display: 'block' })
    .to(replace[index], { opacity: 1, duration: 0.5 })

    replace[index].style.height = '70%';
    replace[index].style.width = '70%';
    imageCont[index].style.display = 'grid';
    imageCont[index].style.placeItems = 'center';
}
);
});

let btn = document.querySelector('.play');
let frame = document.querySelector('.frame');

btn.addEventListener('click', () => {
    frame.classList.toggle('inactive');
    gsap.from(frame, { opacity: 0, xPercent:-100 ,duration: 1 });
});


let question = document.querySelectorAll('.math ol li');
let answers = document.querySelectorAll('.answer');
let equals = document.querySelectorAll('.equal');

question.forEach((item, index) => {
    item.addEventListener('click', () => {
        equals[index].style.display = 'block';
        answers[index].style.display = 'block';
        gsap.from(equals[index], { opacity: 0, duration: 1 });
        gsap.from(answers[index], { opacity: 0, duration: 1 });
    });
});
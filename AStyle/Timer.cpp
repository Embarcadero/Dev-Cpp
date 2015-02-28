#include "Timer.h"

Timer::Timer() {
	this->miliseconds = -1;
	this->running = false;
	this->OnTimer = new TimeEvent();
	if(scene) {
		scene->timers->Add(this); // add self
	}
}
Timer::~Timer() {
	delete OnTimer;
	if(scene) {
		scene->timers->Delete(this); // remove self
	}
}
void Timer::AddEvent(void (*function)(void* sender,double data),void* sender) {
	OnTimer->Add(function,sender);
}
void Timer::Set(int miliseconds,bool running) {
	this->miliseconds = miliseconds;
	this->running = running;
}
void Timer::OnUpdateTime(double dt) {
	if(IsRunning()) {
		miliseconds -= (dt * 1000);
		if(miliseconds <= 0) {
			OnTimer->Execute(0); // unused parameters
		}
	}
}
bool Timer::IsRunning() {
	return running;
}
bool Timer::IsFinished() {
	return (miliseconds <= 0) and IsRunning();
}
int Timer::MilisLeft() {
	if(IsRunning()) {
		return miliseconds;
	} else {
		return -1;
	}
}

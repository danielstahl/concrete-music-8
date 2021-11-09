package net.soundmining

import net.soundmining.ConcreteMusic8.{PEN_LID_HIT, PEN_LID_RATTLE, PEN_LID_SCRATCH, client, soundPlays}
import net.soundmining.modular.ModularInstrument.AudioInstrument
import net.soundmining.modular.ModularSynth
import net.soundmining.modular.ModularSynth.{bandRejectFilter, controlMultiply, highPassFilter, lineControl, lowPassFilter, monoDelay, relativePercControl, ringModulate, sawOsc, sineControl, sineOsc, staticAudioBus, staticControl}
import net.soundmining.synth.Instrument
import net.soundmining.synth.Instrument.{EFFECT, TAIL_ACTION}
import net.soundmining.synth.Utils.absoluteTimeToMillis

object Test {
  /*
    * Pen lid hit two strongest frequencies are 3610 and 5226
    * Pen lid rattle 3728.5, 4990, 6278, 9211
    * Pen lid scratch 985, 2365, 5274
    * */
  def playPenLidRate(): Unit = {
    client.resetClock

    val penLidlowRate = 3610.0 / 5226.0

    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 1.0)
      .splay(0.2, -0.2)
      .play(0, 0)


    soundPlays.mono(PEN_LID_HIT)
      .playMono(penLidlowRate, 1.0)
      .splay(0.2, 0.2)
      .play(2, 0)

    val penLidRattleLowRate = 3728.5 / 4990

    soundPlays.mono(PEN_LID_RATTLE)
      .playMono(1.0, 1.0)
      .splay(0.2, -0.2)
      .play(4, 0)


    soundPlays.mono(PEN_LID_RATTLE)
      .playMono(penLidRattleLowRate, 1.0)
      .splay(0.2, 0.2)
      .play(6, 0)

    val penLidScratchLowRate = 985.0 / 2365.0

    soundPlays.mono(PEN_LID_SCRATCH)
      .playMono(1.0, 1.0)
      .splay(0.2, -0.2)
      .play(8, 0)

    soundPlays.mono(PEN_LID_SCRATCH)
      .playMono(penLidScratchLowRate, 1.0)
      .splay(0.2, 0.2)
      .play(10, 0)
  }

  def playLongTremolo(): Unit = {

    playSawLongRing(0, 985, 5274, 13, 0.5, 0.5, (10.0, 10.5), 0.9)

    playSawLongRing(5, 985, 5274, 13, -0.5, 0.5, (10.3, 9.9), 0.9)

    def playSawLongRing(start: Double, freq1: Double, freq2: Double, dur: Double, pan: Double, amp: Double, speed: (Double, Double), min: Double): Unit = {
      val pulse = sawOsc(
        controlMultiply(relativePercControl(0.001, amp, 0.5, Right(Instrument.SINE)),
          sineControl(lineControl(speed._1, speed._2), min, 1.0)),
        staticControl(freq1))
        .addAction(TAIL_ACTION)
      val ring = ringModulate(pulse, staticControl(freq2))
        .addAction(TAIL_ACTION)

      val splay = ModularSynth.splay(ring, staticControl(0.5), centerBus = staticControl(pan))
        .addAction(TAIL_ACTION)
        .withNrOfChannels(2)

      splay.getOutputBus.staticBus(0)
      val graph = splay.buildGraph(start, dur, splay.graph(Seq()))
      client.send(client.newBundle(absoluteTimeToMillis(start), graph))
    }

  }

  def playLongFilter(): Unit = {

    client.resetClock

    // Pen lid rattle 3728.5, 4990, 6278, 9211
    soundPlays.mono(PEN_LID_RATTLE)
      .playMono(1.0, 1.0)
      .ring(4990)
      .splay(0.2, 0)
      .play(1, 0)

    playSawLongRing(0, 3728.5, 4990, 5, 0.5, (3728.5, 10))
    playSawLongRing(0, 3728.5, 6278, 5, -0.5, (4990, 5), low = Some(3728.5), high = Some(6278))

    def playSawLongRing(start: Double, freq1: Double, freq2: Double, dur: Double, pan: Double, reject: (Double, Double), high: Option[Double] = None, low: Option[Double] = None): Unit = {
      val pulse = sawOsc(relativePercControl(0.001, 1.0, 0.5, Right(Instrument.SINE)), staticControl(freq1))
        .addAction(TAIL_ACTION)
      val ring = ringModulate(pulse, staticControl(freq2))
        .addAction(TAIL_ACTION)


      var filter = bandRejectFilter(ring, staticControl(reject._1), staticControl(reject._2))
        .addAction(TAIL_ACTION)
        .asInstanceOf[AudioInstrument]


      high.foreach(h => filter = highPassFilter(filter, staticControl(h)).addAction(TAIL_ACTION).asInstanceOf[AudioInstrument])
      low.foreach(l => filter = lowPassFilter(filter, staticControl(l)).addAction(TAIL_ACTION).asInstanceOf[AudioInstrument])

      val splay = ModularSynth.splay(filter, staticControl(0.5), centerBus = staticControl(pan))
        .addAction(TAIL_ACTION)
        .withNrOfChannels(2)

      splay.getOutputBus.staticBus(0)
      val graph = splay.buildGraph(start, dur, splay.graph(Seq()))
      client.send(client.newBundle(absoluteTimeToMillis(start), graph))
    }

  }

  def playLongRing(): Unit = {
    client.resetClock

    //Pen lid hit 3610 and 5226
    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 1.0)
      .ring(3610)
      .splay(0.2, 0)
      .play(0, 0)

    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 1.0)
      .ring(5226)
      .splay(0.2, 0)
      .play(2, 0)

    playSawLongRing(3, 3610, 5226, 5, 0)

    // Pen lid rattle 3728.5, 4990, 6278, 9211
    soundPlays.mono(PEN_LID_RATTLE)
      .playMono(1.0, 1.0)
      .ring(3728.5)
      .splay(0.2, 0)
      .play(7, 0)

    soundPlays.mono(PEN_LID_RATTLE)
      .playMono(1.0, 1.0)
      .ring(4990)
      .splay(0.2, 0)
      .play(9, 0)

    playSawLongRing(11, 3728.5, 4990, 5, 0)

    soundPlays.mono(PEN_LID_RATTLE)
      .playMono(1.0, 1.0)
      .ring(6278)
      .splay(0.2, 0)
      .play(16, 0)

    playSawLongRing(18, 3728.5, 6278, 5, 0)

    soundPlays.mono(PEN_LID_RATTLE)
      .playMono(1.0, 1.0)
      .ring(9211)
      .splay(0.2, 0)
      .play(23, 0)

    playSawLongRing(25, 3728.5, 9211, 5, 0)

    // Pen lid scratch 985, 2365, 5274
    soundPlays.mono(PEN_LID_SCRATCH)
      .playMono(1.0, 1.0)
      .ring(985)
      .splay(0.2, 0)
      .play(30, 0)

    soundPlays.mono(PEN_LID_SCRATCH)
      .playMono(1.0, 1.0)
      .ring(2365)
      .splay(0.2, 0)
      .play(32, 0)

    playSawLongRing(34, 985, 2365, 5, 0)

    soundPlays.mono(PEN_LID_SCRATCH)
      .playMono(1.0, 1.0)
      .ring(5274)
      .splay(0.2, 0)
      .play(39, 0)

    playSawLongRing(41, 985, 5274, 5, 0)

    def playSineLongRing(start: Double, freq1: Double, freq2: Double, dur: Double, pan: Double): Unit = {
      val sine = sineOsc(relativePercControl(0.001, 1.0, 0.5, Right(Instrument.SINE)), staticControl(freq1))
        .addAction(TAIL_ACTION)
      val ring = ringModulate(sine, staticControl(freq2))
        .addAction(TAIL_ACTION)

      val splay = ModularSynth.splay(ring, staticControl(0.5), centerBus = staticControl(pan))
        .addAction(TAIL_ACTION)
        .withNrOfChannels(2)

      splay.getOutputBus.staticBus(0)
      val graph = splay.buildGraph(start, dur, splay.graph(Seq()))
      client.send(client.newBundle(absoluteTimeToMillis(start), graph))
    }

    def playSawLongRing(start: Double, freq1: Double, freq2: Double, dur: Double, pan: Double): Unit = {
      val pulse = sawOsc(relativePercControl(0.001, 1.0, 0.5, Right(Instrument.SINE)), staticControl(freq1))
        .addAction(TAIL_ACTION)
      val ring = ringModulate(pulse, staticControl(freq2))
        .addAction(TAIL_ACTION)

      val splay = ModularSynth.splay(ring, staticControl(0.5), centerBus = staticControl(pan))
        .addAction(TAIL_ACTION)
        .withNrOfChannels(2)

      splay.getOutputBus.staticBus(0)
      val graph = splay.buildGraph(start, dur, splay.graph(Seq()))
      client.send(client.newBundle(absoluteTimeToMillis(start), graph))
    }
  }

  def playPenLidRing(): Unit = {
    client.resetClock

    //Pen lid hit 3610 and 5226
    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 1.0)
      .splay(0.2, 0)
      .play(0, 0)

    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 1.0)
      .ring(3610)
      .splay(0.2, -0.2)
      .play(2, 0)

    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 1.0)
      .ring(5226)
      .splay(0.2, 0.2)
      .play(4, 0)

    // Pen lid rattle 3728.5, 4990, 6278, 9211
    soundPlays.mono(PEN_LID_RATTLE)
      .playMono(1.0, 1.0)
      .splay(0.2, 0)
      .play(6, 0)

    soundPlays.mono(PEN_LID_RATTLE)
      .playMono(1.0, 1.0)
      .ring(3728.5)
      .splay(0.2, -0.2)
      .play(8, 0)

    soundPlays.mono(PEN_LID_RATTLE)
      .playMono(1.0, 1.0)
      .ring(4990)
      .splay(0.2, 0.2)
      .play(10, 0)

    soundPlays.mono(PEN_LID_RATTLE)
      .playMono(1.0, 1.0)
      .ring(6278)
      .splay(0.2, -0.2)
      .play(12, 0)

    soundPlays.mono(PEN_LID_RATTLE)
      .playMono(1.0, 1.0)
      .ring(9211)
      .splay(0.2, 0.2)
      .play(14, 0)

    // Pen lid scratch 985, 2365, 5274
    soundPlays.mono(PEN_LID_SCRATCH)
      .playMono(1.0, 1.0)
      .splay(0.2, 0)
      .play(16, 0)

    soundPlays.mono(PEN_LID_SCRATCH)
      .playMono(1.0, 1.0)
      .ring(985)
      .splay(0.2, -0.2)
      .play(18, 0)

    soundPlays.mono(PEN_LID_SCRATCH)
      .playMono(1.0, 1.0)
      .ring(2365)
      .splay(0.2, 0.2)
      .play(20, 0)

    soundPlays.mono(PEN_LID_SCRATCH)
      .playMono(1.0, 1.0)
      .ring(5274)
      .splay(0.2, 0)
      .play(22, 0)
  }

  def playMovingRing(): Unit = {
    client.resetClock


    // Pen lid rattle 3728.5, 4990, 6278, 9211
    // Pen lid scratch 985, 2365, 5274

    soundPlays.mono(PEN_LID_SCRATCH)
      .playMono(1.0, 1.0)
      .ring(985, 2365)
      .splay(0.2, -0.2, 0.2)
      .play(0, 0)

    soundPlays.mono(PEN_LID_SCRATCH)
      .playMono(1.0, 1.0)
      .ring(2365, 5274)
      .splay(0.2, 0.2, -0.2)
      .play(2, 0)
  }

  def combineRing(): Unit = {
    client.resetClock

    //Pen lid hit 3610 and 5226
    // Pen lid rattle 3728.5, 4990, 6278, 9211
    // Pen lid scratch 985, 2365, 5274

    soundPlays.mono(PEN_LID_RATTLE)
      .playMono(1.0, 1.0)
      .ring(3728.5)
      .splay(0.2, -0.2)
      .play(0, 0)

    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 1.0)
      .ring(3728.5)
      .splay(0.2, 0.2)
      .play(1, 0)

    soundPlays.mono(PEN_LID_SCRATCH)
      .playMono(1.0, 1.0)
      .ring(985)
      .splay(0.2, -0.6)
      .play(2, 0)

    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 1.0)
      .ring(985)
      .splay(0.2, 0.6)
      .play(3, 0)


    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 1.0)
      .ring(3610)
      .splay(0.2, -0.6)
      .play(4, 0)

    soundPlays.mono(PEN_LID_RATTLE)
      .playMono(1.0, 1.0)
      .ring(3610)
      .splay(0.2, 0.6)
      .play(4, 0)


    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 1.0)
      .ring(5226)
      .splay(0.2, 0.6)
      .play(6, 0)

    soundPlays.mono(PEN_LID_SCRATCH)
      .playMono(1.0, 1.0)
      .ring(5226)
      .splay(0.2, -0.6)
      .play(6, 0)
  }

  def playDelay(): Unit = {
    client.resetClock

    val delayAudioBus = staticAudioBus()
    val delay = monoDelay(delayAudioBus, staticControl(1), 0.5, 3)
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)

    val ring = ringModulate(delay, staticControl(3610))
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)

    val splay = ModularSynth.splay(ring, staticControl(0.5), centerBus = staticControl(0))
      .addAction(TAIL_ACTION)
      .nodeId(EFFECT)
      .withNrOfChannels(2)

    splay.getOutputBus.staticBus(0)
    val graph = splay.buildGraph(0, 15, splay.graph(Seq()))
    client.send(client.newBundle(0, graph))


    // 5226
    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 1.0)
      .play(0, delayAudioBus.getOutputBus.busValue.get, realOutput = false)


    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 1.0)
      .ring(5226)
      .play(0.27, delayAudioBus.getOutputBus.busValue.get, realOutput = false)

  }

  def playPenLid(): Unit = {
    client.resetClock

    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 1.0)
      .splay(0.2, -0.2)
      .play(0, 0)

    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 2.0)
      .lowPass(1000)
      .splay(0.2, -0.2)
      .play(0.5, 0)

    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 1.0)
      .splay(0.2, -0.2)
      .play(2, 0)

    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 2.0)
      .lowPass(1000)
      .splay(0.2, -0.2)
      .play(2.5, 0)

    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 2.0)
      .highPass(7000)
      .splay(0.2, -0.2)
      .play(2.6, 0)

    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 2.0)
      .highPass(11000)
      .splay(0.2, -0.2)
      .play(3, 0)

    soundPlays.mono(PEN_LID_HIT)
      .playMono(1.0, 2.0)
      .highPass(15000)
      .splay(0.2, -0.2)
      .play(3.5, 0)


    soundPlays.mono(PEN_LID_SCRATCH)
      .playMono(1.01, 2.0)
      .highPass(15000)
      .splay(0.2, 0.2)
      .play(4, 0)

    soundPlays.mono(PEN_LID_SCRATCH)
      .playMono(0.99, 2.0)
      .highPass(15000)
      .splay(0.2, -0.2)
      .play(4, 0)


    soundPlays.mono(PEN_LID_RATTLE)
      .playMono(1.01, 2.0)
      .lowPass(1000)
      .splay(0.2, 0.2)
      .play(6, 0)

    soundPlays.mono(PEN_LID_RATTLE)
      .playMono(0.99, 2.0)
      .highPass(15000)
      .splay(0.2, -0.2)
      .play(6, 0)
    /*

        soundPlays.mono(PEN_LID_RATTLE)
          .playMono(1.0, 1.0)
          .splay(0.2, 0.2)
          .play(1, 0)

        soundPlays.mono(PEN_LID_SCRATCH)
          .playMono(1.0, 1.0)
          .splay(0.2, 0.2)
          .play(5, 0)

        soundPlays.mono(PEN_LID_HIT)
          .playMono(1.0, 1.0)
          .splay(0.2, -0.2)
          .play(7, 0)
     */
  }

  /*
  ("chair-drag-1", 0, 0.6), ("chair-drag-2", 0.5, -0.6)
  ("chest-handle", 4, 0.6), ("chest-lid", 0, -0.6)
  ("chest-scratch-1", 0.6, 0.5), ("chest-hit", 0, -0.5)
  ("pen-lid-hit", 0.2, 0.5), ("chest-hit", 0, -0.5)
  ("pen-lid-rattle", 0, 0.5), ("chest-scratch-2", 0.2, -0.5)
  ("clock-door", 0, 0.5), ("chest-hit", 0.2, -0.5)
  * */

  def playSound(sound: Int): Unit = {
    client.resetClock

    val sounds: Seq[((String, Double, Double), (String, Double, Double))] = Seq(
      (("chair-drag-1", 0, 0.6), ("chair-drag-2", 0.5, -0.6)),
      (("chest-handle", 4, 0.6), ("chest-lid", 0, -0.6)),
      (("chest-scratch-1", 0.6, 0.5), ("chest-hit", 0, -0.5)),
      (("pen-lid-hit", 0.2, 0.5), ("chest-hit", 0, -0.5)),
      (("pen-lid-rattle", 0, 0.5), ("chest-scratch-2", 0.2, -0.5)),
      (("clock-door", 0, 0.5), ("chest-hit", 0.2, -0.5)))

    val (sound1, sound2) = sounds(sound)

    internalPlay(sound1)
    internalPlay(sound2)

    def internalPlay(sound: (String, Double, Double)): Unit = {
      val (audio, start, pan) = sound

      soundPlays.mono(audio)
        .playMono(1.0, 1.0)
        .splay(0.2, pan)
        .play(start, 0)
    }
  }
}

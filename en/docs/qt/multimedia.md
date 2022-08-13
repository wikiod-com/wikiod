---
title: "Multimedia"
slug: "multimedia"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

Qt Multimedia is a module providing handling of multimedia (audio, video) and also camera and radio functionality.

However, the supported files of QMediaPlayer depends on the platform. Indeed, on windows, QMediaPlayer uses DirectShow, on Linux, it uses GStreamer. So depending on the platform some files may work on Linux but not on Windows or the opposite.

## Video Playback in Qt 5
Let's create very simple video player using QtMultimedia module of Qt 5.

In .pro file of your application you will need the following lines:

    QT += multimedia multimediawidgets

Note that `multimediawidgets` is necessary for usage of `QVideoWidget`.

    #include <QtMultimedia/QMediaPlayer>
    #include <QtMultimedia/QMediaPlaylist>
    #include <QtMultimediaWidgets/QVideoWidget>

    QMediaPlayer *player;
    QVideoWidget *videoWidget;
    QMediaPlaylist *playlist;

    player = new QMediaPlayer;

    playlist = new QMediaPlaylist(player);
    playlist->addMedia(QUrl::fromLocalFile("actualPathHere"));

    videoWidget = new QVideoWidget;
    player->setVideoOutput(videoWidget);

    videoWidget->show();
    player->play();

That's all - after launching the application (if necessary codecs are installed in the system), video file playback will be started.

The same way you can play video from URL in Internet, not just local file.

## Audio Playback in Qt5
As this is an audio, we don't need a QVideoWidget. So we can do:

    _player = new QMediaPlayer(this);
    QUrl file = QUrl::fromLocalFile(QFileDialog::getOpenFileName(this, tr("Open Music"), "", tr("")));
    if (file.url() == "")
        return ;
    _player->setMedia(file);
    _player->setVolume(50);
    _player->play();

in the .h:

    QMediaPlayer *_player;

this will open a dialog where you can choose your music and it will play it.


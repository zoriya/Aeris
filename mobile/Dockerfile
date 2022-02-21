FROM androidsdk/android-31:latest

# Download Flutter SDK
RUN git clone https://github.com/flutter/flutter.git
ENV PATH "$PATH:/opt/android-sdk-linux/flutter/bin"

# Run basic check to download Dark SDK
RUN flutter upgrade && flutter doctor

COPY . .
RUN flutter pub get

RUN env
RUN ls -l 
# Generate traduction files
RUN flutter gen-l10n
# Generate launcher icon
RUN flutter pub run flutter_launcher_icons:main
# Generate native splashscreen
RUN flutter pub run flutter_native_splash:create
RUN flutter build apk lib/src/main.dart
CMD mv ./build/app/outputs/flutter-apk/app-release.apk /dist/aeris_android.apk

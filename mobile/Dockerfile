FROM androidsdk/android-30:latest

# Download Flutter SDK
RUN git clone https://github.com/flutter/flutter.git
ENV PATH "$PATH:/opt/android-sdk-linux/flutter/bin"

# Run basic check to download Dark SDK
RUN flutter doctor

COPY pubspec.* ./
RUN flutter pub get

COPY . .
RUN ls -l 
# Generate launcher icon
RUN flutter pub run flutter_launcher_icons:main
# Generate traduction files
RUN flutter gen-l10n
# Generate native splashscreen
RUN flutter pub run flutter_native_splash:create
RUN flutter build apk lib/src/main.dart
CMD mv ./build/app/outputs/flutter-apk/app-release.apk /dist/aeris_android.apk

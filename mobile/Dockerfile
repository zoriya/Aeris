FROM androidsdk/android-31:latest
WORKDIR /

# Download Flutter SDK
RUN git clone https://github.com/flutter/flutter.git
# Using Stable flutter version
RUN git -C flutter checkout stable
ENV PATH "/flutter/bin:$PATH"

# Run basic check to download Dark SDK
RUN flutter upgrade && flutter doctor

RUN which flutter

COPY pubspec.yaml pubspec.lock ./
RUN flutter pub get

COPY . .
# Generate traduction files
RUN flutter gen-l10n
# Generate launcher icon
RUN flutter pub run flutter_launcher_icons:main
# Generate native splashscreen
RUN flutter pub run flutter_native_splash:create
RUN flutter build apk lib/main.dart
CMD cp ./build/app/outputs/flutter-apk/app-release.apk /dist/aeris_android.apk

import './globals.css';
import { IBM_Plex_Sans, Space_Grotesk } from 'next/font/google';

const sans = IBM_Plex_Sans({ subsets: ['latin'], weight: ['400', '500', '600', '700'], variable: '--font-sans' });
const display = Space_Grotesk({ subsets: ['latin'], weight: ['500', '700'], variable: '--font-display' });

export const metadata = {
  title: 'TPIA College Records',
  description: 'Texas Public Information Act tracker for Texas public colleges and universities.',
};

export default function RootLayout({ children }) {
  return (
    <html lang="en" className={`${sans.variable} ${display.variable}`}>
      <body>{children}</body>
    </html>
  );
}

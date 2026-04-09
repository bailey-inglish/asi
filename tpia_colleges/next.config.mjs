/** @type {import('next').NextConfig} */
const nextConfig = {
  output: 'standalone',
  outputFileTracingIncludes: {
    '/*': ['./templates/**/*.txt'],
  },
};

export default nextConfig;
